--
--  Copyright (C) 2022-2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNATCOLL.Opt_Parse; use GNATCOLL.Opt_Parse;

with LAL_Refactor; use LAL_Refactor;

with LAL_Refactor.Sort_Dependencies; use LAL_Refactor.Sort_Dependencies;

with Langkit_Support.Slocs; use Langkit_Support.Slocs;

with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Common; use Libadalang.Common;
with Libadalang.Helpers; use Libadalang.Helpers;

--  This procedure defines the Sort Dependencies Tool

--  Usage:
--  sort_dependencies --project <project> --source <source> [--start-line]
--  [--start-column] [--end-line] [--end-column] [--no-separator]
--
--  --project, -P   Project file
--  --source, -S    Source code file to sort dependencies
--  --start-line    Start line of the prelude slice
--  --start-column  Start column of the prelude slice
--  --end-line      End line of the prelude slice
--  --end-column    End column of the prelude slice
--  --no-separator  Do not separate clauses groups by an empty line
--
--   See Help_Message below.

procedure Sort_Dependencies is
   Help_Message : constant String :=
     "Sort Dependencies"
     & LF
     & LF
     & "Note:"
     & LF
     & "--start-line, --start-column, --end-line, --end-column define the "
     & "source location range of the prelude slice. If one of them is invalid "
     & "or undefined, then the rest of them are ignored and the whole prelude "
     & "is sorted.";

   procedure Sort_Dependencies_App_Setup
     (Context : App_Context; Jobs : App_Job_Context_Array);
   --  This procedure is called right after command line options are parsed,
   --  the project is loaded (if present) and the list of files to process
   --  is computed.

   package Sort_Dependencies_App is new Libadalang.Helpers.App
     (Name             => "Sort_Dependencies",
      Description      => Help_Message,
      App_setup        => Sort_Dependencies_App_Setup);

   package Args is
      package Source is new GNATCOLL.Opt_Parse.Parse_Option
        (Parser      => Sort_Dependencies_App.Args.Parser,
         Short       => "-S",
         Long        => "--source",
         Help        => "Source code file to sort dependencies",
         Arg_Type    => Unbounded_String,
         Convert     => To_Unbounded_String,
         Default_Val => Null_Unbounded_String,
         Enabled     => True);

      package Start_Line is new GNATCOLL.Opt_Parse.Parse_Option
        (Parser      => Sort_Dependencies_App.Args.Parser,
         Long        => "--start-line",
         Help        => "Start line of the prelude slice",
         Arg_Type    => Line_Number,
         Convert     => Line_Number'Value,
         Default_Val => 0,
         Enabled     => True);

      package Start_Column is new GNATCOLL.Opt_Parse.Parse_Option
        (Parser      => Sort_Dependencies_App.Args.Parser,
         Long        => "--start-column",
         Help        => "Start column of the prelude slice",
         Arg_Type    => Column_Number,
         Convert     => Column_Number'Value,
         Default_Val => 0,
         Enabled     => True);

      package End_Line is new GNATCOLL.Opt_Parse.Parse_Option
        (Parser      => Sort_Dependencies_App.Args.Parser,
         Long        => "--end-line",
         Help        => "End line of the prelude slice",
         Arg_Type    => Line_Number,
         Convert     => Line_Number'Value,
         Default_Val => 0,
         Enabled     => True);

      package End_Column is new GNATCOLL.Opt_Parse.Parse_Option
        (Parser      => Sort_Dependencies_App.Args.Parser,
         Long        => "--end-column",
         Help        => "End column of the prelude slice",
         Arg_Type    => Column_Number,
         Convert     => Column_Number'Value,
         Default_Val => 0,
         Enabled     => True);

      package No_Separator is new GNATCOLL.Opt_Parse.Parse_Flag
        (Parser  => Sort_Dependencies_App.Args.Parser,
         Long    => "--no-separator",
         Help    => "Do not separate clauses groups by an empty line",
         Enabled => True);

   end Args;

   ---------------------------------
   -- Sort_Dependencies_App_Setup --
   ---------------------------------

   procedure Sort_Dependencies_App_Setup
     (Context : App_Context;
      Jobs    : App_Job_Context_Array)
   is
      pragma Unreferenced (Context);

      Source_File : constant String := To_String (Args.Source.Get);
      Unit        : constant Analysis_Unit :=
        Jobs (1).Analysis_Ctx.Get_From_File (Source_File);

      Prelude_Slice_Sloc_Range : constant Source_Location_Range :=
        (if Args.Start_Line.Get /= 0
           and then Args.Start_Column.Get /= 0
           and then Args.End_Line.Get /= 0
           and then Args.End_Column.Get /= 0
         then
           Source_Location_Range'
             (Start_Line   => Args.Start_Line.Get,
              Start_Column => Args.Start_Column.Get,
              End_Line     => Args.End_Line.Get,
              End_Column   => Args.End_Column.Get)
         else No_Source_Location_Range);

      No_Separator : constant Boolean := Args.No_Separator.Get;

   begin
      if Unit.Root.Kind in Ada_Compilation_Unit_List then
         for Compilation_Unit of Unit.Root.As_Compilation_Unit_List loop
            if Prelude_Slice_Sloc_Range = No_Source_Location_Range then
               Print
                 (Create_Dependencies_Sorter
                    (Compilation_Unit.As_Compilation_Unit,
                     No_Separator)
                    .Refactor (null));

            else
               Print
                 (Create_Dependencies_Sorter
                    (Compilation_Unit.As_Compilation_Unit,
                     Prelude_Slice_Sloc_Range,
                     No_Separator)
                    .Refactor (null));
            end if;
         end loop;

      elsif Unit.Root.Kind in Ada_Compilation_Unit then
         if Prelude_Slice_Sloc_Range = No_Source_Location_Range then
            Print
              (Create_Dependencies_Sorter
                 (Unit.Root.As_Compilation_Unit, No_Separator)
                 .Refactor (null));

         else
            Print
              (Create_Dependencies_Sorter
                 (Unit.Root.As_Compilation_Unit,
                  Prelude_Slice_Sloc_Range,
                  No_Separator)
                 .Refactor (null));
         end if;
      end if;
   end Sort_Dependencies_App_Setup;

begin
   Sort_Dependencies_App.Run;
end Sort_Dependencies;
