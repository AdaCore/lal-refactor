--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNATCOLL.Opt_Parse; use GNATCOLL.Opt_Parse;

with LAL_Refactor; use LAL_Refactor;

with LAL_Refactor.Sort_Dependencies; use LAL_Refactor.Sort_Dependencies;

with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Common; use Libadalang.Common;
with Libadalang.Helpers; use Libadalang.Helpers;

--  This procedure defines the Sort Dependencies Tool

--  Usage:
--  sort_dependencies --project <project> --source <source> [--no-separator]
--
--  --project, -P   Project file
--  --source, -S    Source code file to sort dependencies
--  --no-separator  Do not separate clauses groups by an empty line

procedure Sort_Dependencies is

   procedure Sort_Dependencies_App_Setup
     (Context : App_Context;
      Jobs    : App_Job_Context_Array);
   --  This procedure is called right after command line options are parsed,
   --  the project is loaded (if present) and the list of files to process
   --  is computed.

   package Sort_Dependencies_App is new Libadalang.Helpers.App
     (Name             => "Sort_Dependencies",
      Description      => "Sort Dependencies",
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

      No_Separator : constant Boolean := Args.No_Separator.Get;

   begin
      if Unit.Root.Kind in Ada_Compilation_Unit_List then
         for Compilation_Unit of Unit.Root.As_Compilation_Unit_List loop
            Print
              (Create_Dependencies_Sorter
                 (Compilation_Unit.As_Compilation_Unit,
                  No_Separator)
                 .Refactor (null));
         end loop;
      elsif Unit.Root.Kind in Ada_Compilation_Unit then
         Print
           (Create_Dependencies_Sorter
              (Unit.Root.As_Compilation_Unit, No_Separator).Refactor (null));
      end if;
   end Sort_Dependencies_App_Setup;

begin
   Sort_Dependencies_App.Run;
end Sort_Dependencies;
