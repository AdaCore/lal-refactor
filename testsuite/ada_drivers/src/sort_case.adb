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
with LAL_Refactor.Sort_Case;
use LAL_Refactor.Sort_Case;

with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Helpers; use Libadalang.Helpers;

--  This procedure defines the Sort Case Tool.

--  Usage:
--  sort_case -S <source> -PL <line> -PC <column> -OD <order>
--
--  -P,  --project         Project file to use
--  -S,  --source          Source code file of the case statement to sort
--  -PL, --line            Line inside the case statement to sort
--  -PC, --column          Column inside the case statement to sort
--  -OD, --order           Sort order: 'alphabetically' or 'declaration'

procedure Sort_Case is

   type Order_Kind is (Alphabetically, Declaration);

   procedure Sort_Case_App_Setup
     (Context : App_Context;
      Jobs    : App_Job_Context_Array);
   --  This procedure is called right after command line options are parsed,
   --  the project is loaded (if present) and the list of files to process
   --  is computed.

   package Sort_Case_App is new Libadalang.Helpers.App
     (Name             => "Sort_Case",
      Description      => "Sort Case",
      App_setup        => Sort_Case_App_Setup);

   package Args is
      package Source is new GNATCOLL.Opt_Parse.Parse_Option
        (Parser      => Sort_Case_App.Args.Parser,
         Short       => "-S",
         Long        => "--source",
         Help        => "Source code file of the case statement to sort",
         Arg_Type    => Unbounded_String,
         Convert     => To_Unbounded_String,
         Default_Val => Null_Unbounded_String,
         Enabled     => True);

      package Line is new GNATCOLL.Opt_Parse.Parse_Option
        (Parser      => Sort_Case_App.Args.Parser,
         Short       => "-PL",
         Long        => "--line",
         Help        => "Line inside the case statement to sort",
         Arg_Type    => Natural,
         Convert     => Natural'Value,
         Default_Val => 0,
         Enabled     => True);

      package Column is new GNATCOLL.Opt_Parse.Parse_Option
        (Parser      => Sort_Case_App.Args.Parser,
         Short       => "-PC",
         Long        => "--column",
         Help        => "Column inside the case statement to sort",
         Arg_Type    => Natural,
         Convert     => Natural'Value,
         Default_Val => 15,
         Enabled     => True);

      package Order is new GNATCOLL.Opt_Parse.Parse_Enum_Option
        (Parser      => Sort_Case_App.Args.Parser,
         Short       => "-OD",
         Long        => "--order",
         Help        => "Sort order",
         Arg_Type    => Order_Kind,
         Default_Val => Alphabetically,
         Enabled     => True);
   end Args;

   -------------------------
   -- Sort_Case_App_Setup --
   -------------------------

   procedure Sort_Case_App_Setup
     (Context : App_Context;
      Jobs    : App_Job_Context_Array)
   is
      pragma Unreferenced (Context);

      Source_File : constant String := To_String (Args.Source.Get);
      Unit        : constant Analysis_Unit :=
        Jobs (1).Analysis_Ctx.Get_From_File (Source_File);
      Location    : Source_Location :=
        (Line_Number (Args.Line.Get),
         Column_Number (Args.Column.Get));

      Edits       : Refactoring_Edits;

   begin
      if Args.Order.Get = Alphabetically
        and then Is_Sort_Alphabetically_Available (Unit, Location)
      then
         Edits := Create_Alphabetical_Case_Sorter
           (Unit, Location).Refactor (null);

         LAL_Refactor.File_Edits.Apply_Edits (Edits.Text_Edits);
      end if;

      if Args.Order.Get = Declaration
        and then Is_Sort_Declaration_Available (Unit, Location)
      then
         Edits := Create_Declaration_Case_Sorter
           (Unit, Location).Refactor (null);

         LAL_Refactor.File_Edits.Apply_Edits (Edits.Text_Edits);
      end if;
   end Sort_Case_App_Setup;

begin
   Sort_Case_App.Run;
end Sort_Case;
