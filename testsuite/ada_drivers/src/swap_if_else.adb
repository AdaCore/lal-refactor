------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                       Copyright (C) 2025, AdaCore                        --
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

with GNATCOLL.Opt_Parse; use GNATCOLL.Opt_Parse;

with LAL_Refactor.File_Edits;
with Langkit_Support.Slocs; use Langkit_Support.Slocs;

with LAL_Refactor; use LAL_Refactor;
with LAL_Refactor.Swap_If_Else;
use LAL_Refactor.Swap_If_Else;

with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Helpers; use Libadalang.Helpers;

--  This procedure defines the Swap If/elsif/else.

--  Usage:
--  swap_if_not -P <project> -S <source> -SL <start-line> -SC <start-column>
--
--  -P,  --project      Project file to use
--  -S,  --source       Source code file of the statements to swap
--  -SL, --start-line   Line of the first statement to swap
--  -SC, --start-column Column of the first statement to swap

procedure Swap_If_Else is

   procedure Swap_If_Else_App_Setup
     (Context : App_Context;
      Jobs    : App_Job_Context_Array);
   --  This procedure is called right after command line options are parsed,
   --  the project is loaded (if present) and the list of files to process
   --  is computed.

   package Swap_If_Else_App is new Libadalang.Helpers.App
     (Name           => "Swap_If_Else",
      Description    => "Swap If Else",
      App_setup      => Swap_If_Else_App_Setup);

   package Args is
      package Source is new GNATCOLL.Opt_Parse.Parse_Option
        (Parser      => Swap_If_Else_App.Args.Parser,
         Short       => "-S",
         Long        => "--source",
         Help        => "Source code file of the statements to swap",
         Arg_Type    => Unbounded_String,
         Convert     => To_Unbounded_String,
         Default_Val => Null_Unbounded_String,
         Enabled     => True);

      package Start_Line is new GNATCOLL.Opt_Parse.Parse_Option
        (Parser      => Swap_If_Else_App.Args.Parser,
         Short       => "-SL",
         Long        => "--start-line",
         Help        => "Line of the first statement to swap",
         Arg_Type    => Natural,
         Convert     => Natural'Value,
         Default_Val => 0,
         Enabled     => True);

      package Start_Column is new GNATCOLL.Opt_Parse.Parse_Option
        (Parser      => Swap_If_Else_App.Args.Parser,
         Short       => "-SC",
         Long        => "--start-column",
         Help        => "Column of the first statement to swap",
         Arg_Type    => Natural,
         Convert     => Natural'Value,
         Default_Val => 0,
         Enabled     => True);
   end Args;

   ----------------------------
   -- Swap_If_Else_App_Setup --
   ----------------------------

   procedure Swap_If_Else_App_Setup
     (Context : App_Context;
      Jobs    : App_Job_Context_Array)
   is
      pragma Unreferenced (Context);

      Source_File        : constant String := To_String (Args.Source.Get);
      Unit               : constant Analysis_Unit :=
        Jobs (1).Analysis_Ctx.Get_From_File (Source_File);
      Location           : constant Source_Location :=
        (Line_Number (Args.Start_Line.Get),
         Column_Number (Args.Start_Column.Get));

      Edits : Refactoring_Edits;

   begin
      if Is_Swap_Available (Unit, Location) then
         Edits := Create_Swaper (Unit, Location).Refactor (null);

         LAL_Refactor.File_Edits.Apply_Edits (Edits.Text_Edits);
      end if;
   end Swap_If_Else_App_Setup;

begin
   Swap_If_Else_App.Run;
end Swap_If_Else;
