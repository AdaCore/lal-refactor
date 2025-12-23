with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

with GNATCOLL.Opt_Parse;

with Libadalang.Analysis;         use Libadalang.Analysis;
with Libadalang.Helpers;          use Libadalang.Helpers;
with Libadalang.Project_Provider; use Libadalang.Project_Provider;

with LAL_Refactor;                  use LAL_Refactor;
with LAL_Refactor.Generate_Package; use LAL_Refactor.Generate_Package;

--  Generate Package Tool
--
--  Usage:
--  generate_package -P <project_file> -S <package_specification_path>
--
--  -P,  --project        Project file to use
--  -S,  --spec           Package specification filename to read

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

      procedure Run_Refactor (Unit : Analysis_Unit);
      --  Driver to create and run refactoring tool

      function Analysis_Units return Analysis_Unit_Array
      is (Units);
      --  Provide context to Refactor

      ------------------
      -- Run_Refactor --
      ------------------

      procedure Run_Refactor (Unit : Analysis_Unit) is
         Generator : Package_Generator;
         Edits     : Refactoring_Edits := No_Refactoring_Edits;
         Spec      : Base_Package_Decl;
      begin
         if Is_Generate_Package_Available (Unit, Spec) then
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
            Put_Line ("Unable to generate package body for " & Spec_Path);
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

      --  Run test
      Run_Refactor (Main_Unit);
   end Generate_Package_App_Setup;

begin
   Generate_Package_App.Run;
end Generate_Package;
