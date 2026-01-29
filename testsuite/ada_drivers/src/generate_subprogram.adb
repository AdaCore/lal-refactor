------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                    Copyright (C) 2025-2026, AdaCore                      --
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

with Langkit_Support;
with Langkit_Support.Text;  use Langkit_Support.Text;
with Langkit_Support.Slocs; use Langkit_Support.Slocs;

with Libadalang.Analysis;         use Libadalang.Analysis;
with Libadalang.Helpers;          use Libadalang.Helpers;
with Libadalang.Project_Provider; use Libadalang.Project_Provider;

with LAL_Refactor;                     use LAL_Refactor;
with LAL_Refactor.Generate_Subprogram; use LAL_Refactor.Generate_Subprogram;

--  Generate Subprogram Tool
--
--  Usage:
--  generate_subprogram -P <project_file> -S <source_file> -SL <line_number>
--  -SC <column_number>
--
--  -P,  --project          Project file to use
--  -S,  --source           Source code file of the node
--  -SL, --line             Start Line number of the node
--  -SC, --column           Start Column number of the node

procedure Generate_Subprogram is

   procedure Generate_Subprogram_App_Setup
     (Context : App_Context; Jobs : App_Job_Context_Array);
   --  Main procedure of this program

   package Generate_Subprogram_App is new
     Libadalang.Helpers.App
       (Name        => "generate_subprogram",
        Description => "Generate Subprogram",
        App_setup   => Generate_Subprogram_App_Setup);

   --  CLI args passed to driver
   package Args is
      package Source is new
        GNATCOLL.Opt_Parse.Parse_Option
          (Parser      => Generate_Subprogram_App.Args.Parser,
           Short       => "-S",
           Long        => "--source",
           Help        => "Source code file of the subprogram declaration",
           Arg_Type    => Unbounded_String,
           Convert     => To_Unbounded_String,
           Default_Val => Null_Unbounded_String,
           Enabled     => True);

      package Start_Line is new
        GNATCOLL.Opt_Parse.Parse_Option
          (Parser      => Generate_Subprogram_App.Args.Parser,
           Short       => "-SL",
           Long        => "--start-line",
           Help        => "Beginning line of subprogram declaration",
           Arg_Type    => Natural,
           Convert     => Natural'Value,
           Default_Val => 1,
           Enabled     => True);

      package Start_Column is new
        GNATCOLL.Opt_Parse.Parse_Option
          (Parser      => Generate_Subprogram_App.Args.Parser,
           Short       => "-SC",
           Long        => "--start-column",
           Help        => "Beginning column of the subprogram declaration",
           Arg_Type    => Natural,
           Convert     => Natural'Value,
           Default_Val => 0,
           Enabled     => True);
   end Args;

   -----------------------------------
   -- Generate_Subprogram_App_Setup --
   -----------------------------------

   procedure Generate_Subprogram_App_Setup
     (Context : App_Context; Jobs : App_Job_Context_Array)
   is
      Source_File : constant String := To_String (Args.Source.Get);

      Sloc : constant Source_Location :=
        (Line_Number (Args.Start_Line.Get),
         Column_Number (Args.Start_Column.Get));

      Files : constant Filename_Vectors.Vector :=
        Source_Files (Context.Provider.Project);

      Main_Unit       : Analysis_Unit;
      Number_Of_Units : constant Positive := Natural (Files.Length);
      Units_Index     : Positive := 1;
      Units           : Analysis_Unit_Array (1 .. Number_Of_Units);

      function Analysis_Units return Analysis_Unit_Array
      is (Units);
      --  Provide context to Refactor

      function Get_Subp_Name (Subp : Subp_Decl) return String
      is (To_UTF8 (Subp.P_Defining_Name.Text));
      --  Name string for debugging

      Target_Subprogram : Subp_Decl := No_Subp_Decl;

      Edits : Refactoring_Edits;

   begin
      Main_Unit := Jobs (1).Analysis_Ctx.Get_From_File (Source_File);

      --  Loop through project source to build LAL diagnostics
      for File of Files loop
         Units (Units_Index) :=
           Main_Unit.Context.Get_From_File (To_String (File));
         Units_Index := Units_Index + 1;
      end loop;
      if Is_Generate_Subprogram_Available (Main_Unit, Sloc, Target_Subprogram)
      then
         declare
            Generator : constant Subprogram_Generator :=
              Create_Subprogram_Generator (Target_Subprogram, Source_File);
            --  Test body generation in the same file as declaration

         begin
            Put_Line
              ("Generating body for subprogram '"
               & Get_Subp_Name (Target_Subprogram)
               & "' declared in "
               & Source_File);

            Edits := Generator.Refactor (Analysis_Units'Access);

            Print (Edits);
            New_Line;
         end;

      else
         Put_Line
           ("Could not generate body for subprogram declared in "
            & Source_File
            & " "
            & Sloc.Image);
         New_Line;
      end if;
   end Generate_Subprogram_App_Setup;

begin
   Generate_Subprogram_App.Run;
end Generate_Subprogram;
