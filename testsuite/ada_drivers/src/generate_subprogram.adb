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

with Ada.Strings.Unbounded;            use Ada.Strings.Unbounded;
with Ada.Text_IO;                      use Ada.Text_IO;

with GNATCOLL.Opt_Parse;

with Langkit_Support;
with Langkit_Support.Text;             use Langkit_Support.Text;
with Langkit_Support.Slocs;            use Langkit_Support.Slocs;

with Libadalang.Analysis;              use Libadalang.Analysis;
with Libadalang.Helpers;               use Libadalang.Helpers;
with Libadalang.Project_Provider;      use Libadalang.Project_Provider;
with LAL_Refactor;                     use LAL_Refactor;
with LAL_Refactor.Generate_Subprogram;

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

      procedure Test_Line (Unit : Analysis_Unit; L : Line_Number);
      --  Test refactoring available and equivalent everywhere on this line

      procedure Test_Sloc (Unit : Analysis_Unit; Sloc : Source_Location);
      --  Test refactoring only for one location

      ---------------
      -- Test_Line --
      ---------------

      procedure Test_Line (Unit : Analysis_Unit; L : Line_Number) is
         Line_Text  : constant Text_Type := Unit.Get_Line (Positive (L));
         Line_Start : constant Source_Location := (L, Column_Number (1));

         C                 : Positive := 2;
         Point             : Source_Location := (L, Column_Number (C));
         Subp              : Subp_Decl := No_Subp_Decl;
         Base_Edits, Edits : Refactoring_Edits := No_Refactoring_Edits;

         use LAL_Refactor.Generate_Subprogram;
      begin
         Base_Edits :=
           (if Is_Generate_Subprogram_Available (Unit, Line_Start, Subp)
            then
              Create_Subprogram_Generator (Subp).Refactor
                (Analysis_Units'Access)
            else No_Refactoring_Edits);
         --  If no refactoring available, check this is true across entire line
         --  If refactoring successful, check the result is the same

         while C < Line_Text'Length
           and then Is_Generate_Subprogram_Available (Unit, Point, Subp)
         loop
            Edits :=
              Create_Subprogram_Generator (Subp).Refactor
                (Analysis_Units'Access);
            exit when Base_Edits /= Edits;
            C := C + 1;
            Point.Column := Column_Number (C);
         end loop;

         if Base_Edits /= Edits then
            New_Line;
            Put_Line ("Generate Subprogram results differ on line " & L'Img);
            Put_Line ("Expected results at " & Line_Start.Image);
            Print (Base_Edits);
            Put_Line ("Unexpected result at " & Point.Image);
            Print (Edits);
         elsif Base_Edits.Has_Failed then
            Put_Line ("Generate Subprogram unavailable on line " & L'Img);
         elsif C < Line_Text'Length then
            Put_Line
              ("Generate Subprogram unavailable at location " & Point.Image);
         else
            New_Line;
            Put_Line ("Generate Subprogram available on line " & L'Img);
            Print (Edits);
         end if;
      end Test_Line;

      ---------------
      -- Test_Sloc --
      ---------------

      procedure Test_Sloc (Unit : Analysis_Unit; Sloc : Source_Location) is
         Subp  : Subp_Decl := No_Subp_Decl;
         Edits : Refactoring_Edits := No_Refactoring_Edits;
         use LAL_Refactor.Generate_Subprogram;
      begin
         if Is_Generate_Subprogram_Available (Unit, Sloc, Subp) then
            Edits :=
              Create_Subprogram_Generator (Subp).Refactor
                (Analysis_Units'Access);
            New_Line;
            Put_Line
              ("Generate Subprogram refactoring for subprogram "
               & Get_Subp_Name (Subp));
            Print (Edits);
         else
            Put_Line
              ("Generate Subprogram unavailable at location " & Sloc.Image);
         end if;
      end Test_Sloc;

   begin
      Main_Unit := Jobs (1).Analysis_Ctx.Get_From_File (Source_File);

      --  Loop through project source to build LAL diagnostics
      for File of Files loop
         Units (Units_Index) :=
           Main_Unit.Context.Get_From_File (To_String (File));
         Units_Index := Units_Index + 1;
      end loop;

      if Sloc.Column = 0
        and then Main_Unit.Get_Line (Positive (Sloc.Line))'Length > 0
      then
         Test_Line (Main_Unit, Sloc.Line);
      else
         Test_Sloc (Main_Unit, Sloc);
      end if;
   end Generate_Subprogram_App_Setup;

begin
   Generate_Subprogram_App.Run;
end Generate_Subprogram;
