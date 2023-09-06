--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Containers;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNATCOLL.Opt_Parse;
with GNATCOLL.VFS; use GNATCOLL.VFS;

with LAL_Refactor;
with LAL_Refactor.Auto_Import; use LAL_Refactor.Auto_Import;

with Langkit_Support.Slocs; use Langkit_Support.Slocs;

with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Helpers; use Libadalang.Helpers;

--  This procedure defines the Refactor Auto Import Tool. Given the location of
--  a Base_Id in a source code file and the project it belongs to, prints
--  all possible with clauses and prefixes that can be added so that a
--  declaration with the same name becomes visible.

--  Usage:
--  auto_import --project <project_file> --source <source_code_file>
--  --line <line_number> --column <column_number>
--
--  --project          Project file to use
--  --source           Source code file of the identifier
--  --line             Line number of the identifier
--  --column           Column number of the identifier

procedure Auto_Import is

   Units    : Analysis_Unit_Vector;
   Unit     : Analysis_Unit;
   Location : Source_Location;

   procedure App_Setup
     (Context : App_Context;
      Jobs    : App_Job_Context_Array);
   --  This procedure is called right after command line options are parsed,
   --  the project is loaded (if present) and the list of files to process
   --  is computed.

   procedure Process_Unit
     (Context : App_Job_Context;
      Unit    : Analysis_Unit);
   --  This procedure will be called once right after a unit is parsed

   procedure Job_Post_Process
     (Context : App_Job_Context);
   --  This procedure will be called once after all units have been parsed.
   --  Note it will be called once per job.

   package App is new Libadalang.Helpers.App
     (Name             => "auto_import",
      Description      => "refactor imports",
      App_setup        => App_Setup,
      Process_Unit     => Process_Unit,
      Job_Post_Process => Job_Post_Process);

   package Source is new GNATCOLL.Opt_Parse.Parse_Option
     (Parser      => App.Args.Parser,
      Long        => "--source",
      Help        => "Source code file of the node",
      Arg_Type    => Ada.Strings.Unbounded.Unbounded_String,
      Convert     => Ada.Strings.Unbounded.To_Unbounded_String,
      Default_Val => Ada.Strings.Unbounded.Null_Unbounded_String,
      Enabled     => True);

   package Line is new GNATCOLL.Opt_Parse.Parse_Option
     (Parser      => App.Args.Parser,
      Long        => "--line",
      Help        => "Line of the node",
      Arg_Type    => Natural,
      Convert     => Natural'Value,
      Default_Val => 1,
      Enabled     => True);

   package Column is new GNATCOLL.Opt_Parse.Parse_Option
     (Parser      => App.Args.Parser,
      Long        => "--column",
      Help        => "Collumn of the node",
      Arg_Type    => Natural,
      Convert     => Natural'Value,
      Default_Val => 1,
      Enabled     => True);

   ---------------
   -- App_Setup --
   ---------------

   procedure App_Setup
     (Context : App_Context;
      Jobs : App_Job_Context_Array)
   is
      Source_File   : constant Ada.Strings.Unbounded.Unbounded_String :=
        Source.Get;
      Line_Number   : constant Natural := Line.Get;
      Column_Number : constant Natural := Column.Get;

      Files : constant GNATCOLL.VFS.File_Array :=
        Context
          .Provider
          .Project
          .Root_Project
          .Get_Environment
          .Predefined_Source_Files;
   begin
      Unit :=
        Jobs (1).Analysis_Ctx.Get_From_File
          (Ada.Strings.Unbounded.To_String (Source_File));
      for F of Files loop
         Units.Append (Jobs (1).Analysis_Ctx.Get_From_File (+F.Full_Name));
      end loop;
      Location :=
        (Line   => Langkit_Support.Slocs.Line_Number (Line_Number),
         Column => Langkit_Support.Slocs.Column_Number (Column_Number));
   end App_Setup;

   ------------------
   -- Process_Unit --
   ------------------

   procedure Process_Unit
     (Context : App_Job_Context;
      Unit    : Analysis_Unit)
   is
      pragma Unreferenced (Context);
   begin
      Units.Append (Unit);
   end Process_Unit;

   ----------------------
   -- Job_Post_Process --
   ----------------------

   procedure Job_Post_Process
     (Context : App_Job_Context)
   is
      pragma Unreferenced (Context);

   begin
      if Unit.Has_Diagnostics then
         --  For the purposes of testing, do nothing if this Unit has any
         --  diagnostics.
         for Diagnostic of Unit.Diagnostics loop
            Ada.Text_IO.Put_Line
              ("Format_GNU_Diagnostics: "
               & Unit.Format_GNU_Diagnostic (Diagnostic));
         end loop;

      else
         declare
            use type Ada.Containers.Count_Type;

            Name              : Libadalang.Analysis.Name;
            Available_Imports : Import_Type_Ordered_Set;

         begin
            if Is_Auto_Import_Available
                 (Unit, Location, Units, Name, Available_Imports)
            then
               Ada.Text_IO.Put_Line ("Available Imports:");
               for Available_Import of Available_Imports loop
                  Ada.Text_IO.Put_Line (Image (Available_Import));
               end loop;
               Ada.Text_IO.New_Line;

               if Available_Imports.Length /= 0 then
                  declare
                     Units_Array :
                       Analysis_Unit_Array (1 .. Integer (Units.Length));

                     function Units_Provider return Analysis_Unit_Array is
                       (Units_Array);

                  begin
                     for J in 1 .. Units.Length loop
                        Units_Array (Integer (J)) :=
                          Units.Element (Integer (J));
                     end loop;
                     Ada.Text_IO.Put_Line ("Edits:");
                     LAL_Refactor.Print
                       (Create_Auto_Importer
                          (Unit, Location, Available_Imports.First_Element)
                          .Refactor (Units_Provider'Access));
                  end;
               end if;
            end if;
         end;
      end if;
   end Job_Post_Process;

begin
   App.Run;
end Auto_Import;
