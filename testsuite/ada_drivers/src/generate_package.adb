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

with Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

with GNATCOLL.Opt_Parse;

with Langkit_Support.Text;
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
--  Can pass a list of multiple SLOCs along with one file
--  to confirm they all produce the same edit
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
           Help        =>
             "Start line of cursor. If no start column provided,"
             & " check cursor position across entire line.",
           Arg_Type    => Natural,
           Convert     => Natural'Value,
           Default_Val => 1,
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
           --  Check this to see if we test entire line or one SLOC
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
      Test_SLOC       : constant Source_Location :=
        (Line   => Line_Number (Args.Start_Line.Get),
         Column => Column_Number (Args.Start_Column.Get));

      function Analysis_Units return Analysis_Unit_Array
      is (Units);
      --  Provide context to Refactor

      function Refactoring_Available_Here
        (Unit  : Analysis_Unit;
         SLOC  : Source_Location;
         Edits : in out Refactoring_Edits) return Boolean;

      --  Take a source location, check whether refactoring is offered here
      --  and write any resulting edit

      procedure Test_Single_Sloc
        (Unit : Analysis_Unit; Test_SLOC : Source_Location);
      --  Driver for normal mode of testing

      procedure Test_Entire_Line
        (Unit : Analysis_Unit; Line : Line_Number);
      --  Test Generate Package available for entire line

      --------------------------------
      -- Refactoring_Available_Here --
      --------------------------------

      function Refactoring_Available_Here
        (Unit  : Analysis_Unit;
         SLOC  : Source_Location;
         Edits : in out Refactoring_Edits) return Boolean
      is
         Spec        : Base_Package_Decl := No_Base_Package_Decl;
         Start_Token : constant Token_Reference := Unit.Lookup_Token (SLOC);
         --  Start_Token.Line
         Node        : constant Ada_Node :=
           Lookup (Unit, Start_Token, Forward);
      begin
         if Is_Generate_Package_Available (Node, Spec) then
            Edits :=
              Build_Package_Generator (Spec).Refactor (Analysis_Units'Access);
            return True;
         else
            return False;
         end if;
      exception
         when E : others =>
            Put_Line (Ada.Exceptions.Exception_Message (E));
            return False;
      end Refactoring_Available_Here;

      procedure Print_Results (Edits : Refactoring_Edits);
      --  Helper to print edits and error messages

      procedure Print_Results (Edits : Refactoring_Edits) is
      begin
         if Has_Failed (Edits) then
            Put_Line ("Unexpected error occurred. Package generation failed.");
            for Refactor_Error of Edits.Diagnostics loop
               Put_Line (Refactor_Error.Info);
            end loop;
         else
            Print (Edits);
         end if;
      end Print_Results;

      ----------------------
      -- Test_Single_Sloc --
      ----------------------

      procedure Test_Single_Sloc
        (Unit : Analysis_Unit; Test_SLOC : Source_Location)
      is
         Edits : Refactoring_Edits := No_Refactoring_Edits;
      begin
         if Refactoring_Available_Here (Unit, Test_SLOC, Edits) then
            Print_Results (Edits);
         else
            Put_Line
              ("Unable to generate package body for "
               & Spec_Path
               & " at "
               & Image (Test_SLOC));
         end if;
         New_Line;
      end Test_Single_Sloc;

      -------------------------
      -- Test_Entire_Line --
      -------------------------

      procedure Test_Entire_Line (Unit : Analysis_Unit; Line : Line_Number)
      is
         --  Check refactoring result for given source location
         Edits, Exp_Edits : Refactoring_Edits := No_Refactoring_Edits;
         Line_Text        : constant String :=
           Langkit_Support.Text.To_UTF8 (Unit.Get_Line (Positive (Line)));
         Test_SLOC        : Source_Location :=
           (Line, Column_Number (Line_Text'First));
         Available        : Boolean;
      begin
         if Line_Text'Length < 1 then
            return;
         end if;
         Available := Refactoring_Available_Here (Unit, Test_SLOC, Edits);
         Exp_Edits := Edits;
         while Test_SLOC.Column < Column_Number (Line_Text'Last)
         loop
            Available := Refactoring_Available_Here (Unit, Test_SLOC, Edits);
            Test_SLOC.Column := Test_SLOC.Column + Column_Number (1);
            exit when not Available or Exp_Edits /= Edits;
         end loop;
         if Test_SLOC.Column /= Column_Number (Line_Text'Last) then
            Put_Line ("Refactoring unavailable at " & Image (Test_SLOC));
         else
            Put_Line
              ("Refactoring available for entire line " & Positive (Line)'Img);
            Print_Results (Exp_Edits);
         end if;
      end Test_Entire_Line;

   begin
      Main_Unit := Jobs (1).Analysis_Ctx.Get_From_File (Spec_Path);

      --  Loop through project source to build LAL diagnostics
      for File of Files loop
         Units (Units_Index) :=
           Main_Unit.Context.Get_From_File (To_String (File));
         Units_Index := Units_Index + 1;
      end loop;

      --  Run test mode
      if Test_SLOC.Column = Column_Number (0) then
         Test_Entire_Line (Main_Unit, Test_SLOC.Line);
      else
         Test_Single_Sloc (Main_Unit, Test_SLOC);
      end if;
   end Generate_Package_App_Setup;

begin
   Generate_Package_App.Run;
end Generate_Package;
