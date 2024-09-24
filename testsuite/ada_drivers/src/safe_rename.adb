------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                    Copyright (C) 2020-2022, AdaCore                      --
--                                                                          --
-- Libadalang Tools  is free software; you can redistribute it and/or modi- --
-- fy  it  under  terms of the  GNU General Public License  as published by --
-- the Free Software Foundation;  either version 3, or (at your option) any --
-- later version. This software  is distributed in the hope that it will be --
-- useful but  WITHOUT  ANY  WARRANTY; without even the implied warranty of --
-- MERCHANTABILITY  or  FITNESS  FOR A PARTICULAR PURPOSE.                  --
--                                                                          --
-- As a special  exception  under  Section 7  of  GPL  version 3,  you are  --
-- granted additional  permissions described in the  GCC  Runtime  Library  --
-- Exception, version 3.1, as published by the Free Software Foundation.    --
--                                                                          --
-- You should have received a copy of the GNU General Public License and a  --
-- copy of the GCC Runtime Library Exception along with this program;  see  --
-- the files COPYING3 and COPYING.RUNTIME respectively.  If not, see        --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.Opt_Parse; use GNATCOLL.Opt_Parse;
with GPR2.Project.Attribute;
with GPR2.Project.Attribute_Index;

with Langkit_Support.Slocs; use Langkit_Support.Slocs;
with Langkit_Support.Text; use Langkit_Support.Text;

with Laltools.Common; use Laltools.Common;
with LAL_Refactor.Safe_Rename; use LAL_Refactor.Safe_Rename;

with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Helpers; use Libadalang.Helpers;
with Libadalang.Project_Provider; use Libadalang.Project_Provider;

with Printers; use Printers;

--  This procedure defines the Refactor Safe Rename Tool. Given the location of
--  an identifier in a source code file, and the project it belongs to, it
--  finds all references of the node's referenced declaration and checks
--  if the rename will cause an issue.

--  Usage:
--  safe_rename -P <project_file> -S <source_code_file> -L <line_number>
--  -R <column_number> -N <new_name> -A <algorithm>
--
--  -P, --project          Project file to use
--  -S, --source           Source code file of the identifier
--  -L, --line             Line number of the identifier
--  -R, --column           Column number of the identifier
--  -N, --new-name         New name
--  -A, --algorithm        Algorithm used to check for rename conflicts:
--                         'map_references' or 'analyse_ast'

procedure Safe_Rename is

   procedure Safe_Rename_App_Setup
     (Context : App_Context;
      Jobs : App_Job_Context_Array);
   --  This procedure is called right after command line options are parsed,
   --  the project is loaded (if present) and the list of files to process
   --  is computed.

   package Safe_Rename_App is new Libadalang.Helpers.App
     (Name             => "safe_rename",
      Description      => "Safe_Rename",
      App_setup        => Safe_Rename_App_Setup);

   package Args is

      package Source is new GNATCOLL.Opt_Parse.Parse_Option
        (Parser      => Safe_Rename_App.Args.Parser,
         Short       => "-S",
         Long        => "--source",
         Help        => "Source code file of the node",
         Arg_Type    => Unbounded_String,
         Convert     => To_Unbounded_String,
         Default_Val => Null_Unbounded_String,
         Enabled     => True);

      package Line is new GNATCOLL.Opt_Parse.Parse_Option
        (Parser      => Safe_Rename_App.Args.Parser,
         Short       => "-L",
         Long        => "--line",
         Help        => "Line of the node",
         Arg_Type    => Natural,
         Convert     => Natural'Value,
         Default_Val => 1,
         Enabled     => True);

      package Column is new GNATCOLL.Opt_Parse.Parse_Option
        (Parser      => Safe_Rename_App.Args.Parser,
         Short       => "-R",
         Long        => "--column",
         Help        => "Column of the node",
         Arg_Type    => Natural,
         Convert     => Natural'Value,
         Default_Val => 1,
         Enabled     => True);

      package New_Name is new GNATCOLL.Opt_Parse.Parse_Option
        (Parser      => Safe_Rename_App.Args.Parser,
         Short       => "-N",
         Long        => "--new-name",
         Help        => "New name",
         Arg_Type    => Unbounded_String,
         Convert     => To_Unbounded_String,
         Default_Val => Null_Unbounded_String,
         Enabled     => True);

      package Algorithm is new GNATCOLL.Opt_Parse.Parse_Enum_Option
        (Parser      => Safe_Rename_App.Args.Parser,
         Short       => "-A",
         Long        => "--algorithm",
         Help        => "Algorithm used to check for rename conflicts: ",
         Arg_Type    => Problem_Finder_Algorithm_Kind,
         Default_Val => Map_References,
         Enabled     => True);
   end Args;

   ---------------------------
   -- Safe_Rename_App_Setup --
   ---------------------------

   procedure Safe_Rename_App_Setup
     (Context : App_Context;
      Jobs : App_Job_Context_Array)
   is
      Source_File : constant String := To_String (Args.Source.Get);
      Sloc        : constant Source_Location :=
        (Line_Number (Args.Line.Get), Column_Number (Args.Column.Get));
      New_Name    : constant String := To_String (Args.New_Name.Get);
      Algorithm   : constant Problem_Finder_Algorithm_Kind :=
        Args.Algorithm.Get;

      Files : constant Filename_Vectors.Vector :=
        Source_Files (Context.Provider.Project);

      package Analysis_Unit_Vectors is new Ada.Containers.Vectors
        (Index_Type   => Positive,
         Element_Type => Analysis_Unit,
         "="          => "=");

      subtype Analysis_Unit_Vector is Analysis_Unit_Vectors.Vector;

      Units     : Analysis_Unit_Vector;
      Main_Unit : Analysis_Unit;
      Node      : Ada_Node;

   begin
      Main_Unit := Jobs (1).Analysis_Ctx.Get_From_File (Source_File);

      Node := Main_Unit.Root.Lookup (Sloc);

      Put_Line ("# Renaming node " & Image (Node.Full_Sloc_Image));

      for File of Files loop
         Units.Append (Jobs (1).Analysis_Ctx.Get_From_File (To_String (File)));
      end loop;

      declare
         function Attribute_Value_Provider_Callback
           (Attribute : GPR2.Q_Attribute_Id;
            Index : String := "";
            Default : String := "";
            Ignored_Use_Extended : Boolean := False)
            return String;
         --  Attribute provider for the project on this Context

         ---------------------------------------
         -- Attribute_Value_Provider_Callback --
         ---------------------------------------

         function Attribute_Value_Provider_Callback
           (Attribute : GPR2.Q_Attribute_Id;
            Index : String := "";
            Default : String := "";
            Ignored_Use_Extended : Boolean := False)
            return String
         is
            Attr : constant GPR2.Project.Attribute.Object :=
              Context
              .Provider
              .Project
              .Root_Project
              .Attribute
                (Name  => Attribute,
                 Index => (if Index = ""
                           then GPR2.Project.Attribute_Index.Undefined
                           else GPR2.Project.Attribute_Index.Create
                                  (GPR2.Value_Type (Index))));
         begin
            return (if Attr.Is_Defined
                    then Attr.Value.Text
                    else Default);
         end Attribute_Value_Provider_Callback;

         Attribute_Value_Provider : constant
            GPR2_Attribute_Value_Provider_Access :=
               Attribute_Value_Provider_Callback'Unrestricted_Access;

         Renamer : constant Safe_Renamer :=
           Create_Safe_Renamer
             (Definition               =>
                Resolve_Name_Precisely (Get_Node_As_Name (Node)),
              New_Name                 =>
                To_Unbounded_Text (To_Text (New_Name)),
              Algorithm                => Algorithm,
              Attribute_Value_Provider => Attribute_Value_Provider);

         Units_Array : Analysis_Unit_Array (1 .. Integer (Units.Length));

         function Analysis_Units return Analysis_Unit_Array is (Units_Array);

      begin
         for J in 1 .. Units.Length loop
            Units_Array (Integer (J)) := Units.Element (Integer (J));
         end loop;

         PP (Renamer.Refactor (Analysis_Units'Access));
      end;

      New_Line;
   end Safe_Rename_App_Setup;

begin
   Safe_Rename_App.Run;
end Safe_Rename;
