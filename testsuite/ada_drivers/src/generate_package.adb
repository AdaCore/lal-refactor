------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                    Copyright (C) 2026, AdaCore                           --
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

with GNATCOLL.Opt_Parse;

with Langkit_Support.Slocs;       use Langkit_Support.Slocs;
with Libadalang.Analysis;         use Libadalang.Analysis;
with Libadalang.Common;           use Libadalang.Common;
with Libadalang.Helpers;          use Libadalang.Helpers;
with Libadalang.Project_Provider; use Libadalang.Project_Provider;

with LAL_Refactor;                  use LAL_Refactor;
with LAL_Refactor.Generate_Package; use LAL_Refactor.Generate_Package;
with LAL_Refactor.Tools;            use LAL_Refactor.Tools;

--  Generate Package Tool
--
--  Usage:
--  generate_package -P <project_file> -S <package_specification_path>
--
--  -P,  --project         Project file to use
--  -S,  --spec            Package specification filename to read
--  -SL  --start-line      Start line to read node from
--  -SC  --start-column    Start column to read node from

procedure Generate_Package is

   procedure Generate_Package_App_Setup
     (Context : App_Context; Jobs : App_Job_Context_Array);
   --  Main procedure of this program

   package Generate_Package_App is new
     App
       (Name        => "generate_package",
        Description => "Generate Package",
        App_setup   => Generate_Package_App_Setup);

   --  CLI args passed to driver
   package Args is
      package Spec is new
        GNATCOLL.Opt_Parse.Parse_Option
          (Parser      => Generate_Package_App.Args.Parser,
           Short       => "-S",
           Long        => "--spec",
           Help        => "Path of package specification",
           Arg_Type    => Unbounded_String,
           Convert     => To_Unbounded_String,
           Default_Val => Null_Unbounded_String,
           Enabled     => True);
      package Start_Line is new
        GNATCOLL.Opt_Parse.Parse_Option
          (Parser      => Generate_Package_App.Args.Parser,
           Short       => "-SL",
           Long        => "--start-line",
           Help        => "Start line of cursor",
           Arg_Type    => Natural,
           Convert     => Natural'Value,
           Default_Val => 0,
           Enabled     => True);
      package Start_Column is new
        GNATCOLL.Opt_Parse.Parse_Option
          (Parser      => Generate_Package_App.Args.Parser,
           Short       => "-SC",
           Long        => "--start-column",
           Help        => "Start column of cursor",
           Arg_Type    => Natural,
           Convert     => Natural'Value,
           Default_Val => 0,
           Enabled     => True);

   end Args;

   --------------------------------
   -- Generate_Package_App_Setup --
   --------------------------------

   procedure Generate_Package_App_Setup
     (Context : App_Context; Jobs : App_Job_Context_Array)
   is

      Spec_Path : constant String := To_String (Args.Spec.Get);
      Files     : constant Filename_Vectors.Vector :=
        Source_Files (Context.Provider.Project);

      Main_Unit       : Analysis_Unit;
      Number_Of_Units : constant Positive := Natural (Files.Length);
      Units_Index     : Positive := 1;
      Units           : Analysis_Unit_Array (1 .. Number_Of_Units);
      SLOC            : constant Source_Location :=
        (Line   => Line_Number (Args.Start_Line.Get),
         Column => Column_Number (Args.Start_Column.Get));
      Start_Token     : Token_Reference;
      Node            : Ada_Node;

      procedure Run_Refactor (Node : Ada_Node);
      --  Driver to create and run refactoring tool

      function Analysis_Units return Analysis_Unit_Array
      is (Units);
      --  Provide context to Refactor

      ------------------
      -- Run_Refactor --
      ------------------

      procedure Run_Refactor (Node : Ada_Node) is
         Generator : Package_Generator;
         Edits     : Refactoring_Edits := No_Refactoring_Edits;
         Spec      : Base_Package_Decl;
      begin
         if Is_Generate_Package_Available (Node, Spec) then
            Generator := Build_Package_Generator (Spec);
            Edits := Generator.Refactor (Analysis_Units'Access);

            if Has_Failed (Edits) then
               Put_Line
                 ("Unexpected error occurred. Package generation failed.");
               for Refactor_Error of Edits.Diagnostics loop
                  Put_Line (Refactor_Error.Info);
               end loop;
            else
               Print (Edits);
            end if;
         else
            Put_Line
              ("Unable to generate package body for "
               & Spec_Path
               & " at "
               & Image (SLOC));
         end if;
         New_Line;
      end Run_Refactor;

   begin
      Main_Unit := Jobs (1).Analysis_Ctx.Get_From_File (Spec_Path);

      --  Loop through project source to build LAL diagnostics
      for File of Files loop
         Units (Units_Index) :=
           Main_Unit.Context.Get_From_File (To_String (File));
         Units_Index := Units_Index + 1;
      end loop;

      Start_Token := Main_Unit.Lookup_Token (SLOC);
      Node := Lookup (Main_Unit, Start_Token, Forward);

      --  Run test
      Run_Refactor (Node);
   end Generate_Package_App_Setup;

begin
   Generate_Package_App.Run;
end Generate_Package;
