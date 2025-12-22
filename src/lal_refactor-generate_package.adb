--
--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  This package contains refactoring tools to generate or update
--  a GNAT-compliant package body file from a package declaration

with Ada.Characters;
with Ada.Characters.Latin_1;
with VSS.Strings.Conversions;

with Langkit_Support.Text; use Langkit_Support.Text;
with LAL_Refactor.Stub_Utils;

package body LAL_Refactor.Generate_Package is
   Tool_Name : constant String := "Generate Package";

   -----------------------------------
   -- Is_Generate_Package_Available --
   -----------------------------------

   function Is_Generate_Package_Available
     (Unit : Analysis_Unit; Spec : out Base_Package_Decl) return Boolean is
   begin
      Spec := To_Package_Decl (Unit);
      return
        (not Spec.Is_Null
         and then not Spec.F_Public_Part.Is_Null
         and then (for some Decl of Spec.F_Public_Part.F_Decls =>
                     Is_Unimplemented_Subprogram (Decl)));
   exception
      when E : others =>
         Refactor_Trace.Trace
           (E, Refactoring_Tool_Refactor_Default_Error_Message (Tool_Name));
         return False;
   end Is_Generate_Package_Available;

   -------------------
   -- Get_Body_Path --
   -------------------

   function Get_Body_Path (From_Spec : Base_Package_Decl) return String is
      S : String := From_Spec.Unit.Get_Filename;
   begin
      if Package_Body_Exists (From_Spec) then
         return From_Spec.P_Body_Part.Unit.Get_Filename;
      else
         --  Really relying on GNAT filename conventions
         S (S'Last) := 'b';
         return S;
      end if;
   end Get_Body_Path;

   -----------------------------
   -- Build_Package_Generator --
   -----------------------------

   function Build_Package_Generator
     (Spec : Base_Package_Decl) return Package_Generator
   is
      function Build_Decl_Map (Spec : Base_Package_Decl) return Decl_Vector;
      --  Build list of only declaration nodes which must be generated

      --------------------
      -- Build_Decl_Map --
      --------------------

      function Build_Decl_Map (Spec : Base_Package_Decl) return Decl_Vector is
         Decls : Decl_Vector := Declaration_Vectors.Empty_Vector;
      begin
         for Decl of Spec.F_Public_Part.F_Decls loop
            if Is_Unimplemented_Subprogram (Decl) then
               Decls.Append (Decl.As_Basic_Subp_Decl);
            end if;
         end loop;
         return Decls;
      end Build_Decl_Map;
   begin
      return
        (Spec        => Spec,
         Subprograms => Build_Decl_Map (Spec),
         Body_Path   => To_Unbounded_String (Get_Body_Path (Spec)));
   end Build_Package_Generator;

   --------------
   -- Refactor --
   --------------

   overriding
   function Refactor
     (Self           : Package_Generator;
      Analysis_Units : access function return Analysis_Unit_Array)
      return Refactoring_Edits
   is
      Package_Edits : Refactoring_Edits := No_Refactoring_Edits;

      procedure Add_Subprogram_Diagnostic
        (Edits : out Refactoring_Edits; Subprogram : Basic_Subp_Decl'Class);
      --  Helper to append errors related to a specific subprogram

      function Get_Insertion_Point
        (Body_Unit : Analysis_Unit := No_Analysis_Unit;
         Decl      : Basic_Subp_Decl'Class := No_Basic_Subp_Decl)
         return Source_Location_Range;
      --  Calculate appropriate insertion point in package body
      --  Text edit maps use insertion point location as map key
      --  thus insertion points must be unique for each edit.
      --  To avoid worrying about line numbers changing,
      --  text edits are placed from the bottom of the file
      --  (highest line number) to the top (lowest).
      --
      --  Easy strategy for updating an existing package body:
      --  insert new subprogram body stubs at the bottom.
      --  This means putting all the new stubs in one edit
      --  so the insertion point is unique.
      --
      --  Complicated strategy: follow the order in which subprograms
      --  are declared in the package specification.
      --  This means grouping new stubs together around existing
      --  subprogram declarations.
      --
      --  e.g. refactoring a package specification like this:
      --
      --  -----------------------------------------------------|
      --  -- Package Spec -|- Package Body -|- Text edits -|-L-|
      --  -----------------------------------------------------|
      --  L2  procedure A; |----------------|- A before B  | 3 |
      --  L5  function  B; |-L3 function B -|--------------|---|
      --  L8  function  C; |-L9-------------|- C after B --| 9 |
      --  L9  procedure D; |----------------|- D after C --| 9 |
      --  -----------------------------------------------------|
      --
      --  should produce the following text edits
      --    1. Insert A at line 3
      --    2. Insert (C + D) at line 9
      --  to be processed in reverse order

      procedure Add_New_Package_Body
        (Edits      : out Refactoring_Edits;
         From_Spec  : Base_Package_Decl;
         With_Decls : Decl_Vector;
         To_Path    : Unbounded_String);
      --  Create edits to go into one new file

      procedure Update_Package_Body
        (Edits      : out Refactoring_Edits;
         From_Decls : Decl_Vector;
         To_Body    : Analysis_Unit);
      --  Text edits to modify an existing package body

      -------------------------------
      -- Add_Subprogram_Diagnostic --
      -------------------------------

      procedure Add_Subprogram_Diagnostic
        (Edits : out Refactoring_Edits; Subprogram : Basic_Subp_Decl'Class) is
      begin
         Edits.Diagnostics.Append
           (Problem
              (Msg  =>
                 "Could not generate subprogram body for "
                 & To_UTF8 (Subprogram.P_Defining_Name.Text),
               File => Subprogram.Unit.Get_Filename,
               SLOC => Subprogram.Sloc_Range));
      end Add_Subprogram_Diagnostic;

      -------------------------
      -- Get_Insertion_Point --
      -------------------------

      function Get_Insertion_Point
        (Body_Unit : Analysis_Unit := No_Analysis_Unit;
         Decl      : Basic_Subp_Decl'Class := No_Basic_Subp_Decl)
         return Source_Location_Range
      is
         pragma Unreferenced (Decl);
         --  Currently defaults to end of package
         --  TODO : update to check Decl placement in Body_Unit
         Last_Line        : constant Line_Number :=
           (if Body_Unit in No_Analysis_Unit
            then Line_Number (2)
            else Sloc_Range (Data (Body_Unit.Last_Token)).End_Line);
         Package_Body_End : constant Source_Location_Range :=
           (Start_Line | End_Line     => Last_Line - Line_Number (1),
            Start_Column | End_Column => Column_Number (1));
      begin
         return Package_Body_End;
      end Get_Insertion_Point;

      --------------------------
      -- Add_New_Package_Body --
      --------------------------

      procedure Add_New_Package_Body
        (Edits      : out Refactoring_Edits;
         From_Spec  : Base_Package_Decl;
         With_Decls : Decl_Vector;
         To_Path    : Unbounded_String)
      is
         --  TODO move this to Stub_Utils
         Package_Name    : constant String := Get_Package_Name (From_Spec);
         LF              : constant Character := Ada.Characters.Latin_1.LF;
         Header          : constant String :=
           "package body " & Package_Name & " is" & LF;
         Footer          : constant String := "end " & Package_Name & ';' & LF;
         Body_Text       : Unbounded_String;
         Subprogram_Stub : Unbounded_String;
         use LAL_Refactor.Stub_Utils;
      begin
         Body_Text.Append (To_Unbounded_String (Header));
         for Decl of With_Decls loop
            Subprogram_Stub :=
              Create_Code_Generator (Decl.As_Subp_Decl)
                .Generate_Subprogram_Body;
            if Subprogram_Stub = Null_Unbounded_String then
               Add_Subprogram_Diagnostic (Edits, Decl);
            else
               Append (Source => Body_Text, New_Item => Subprogram_Stub);
            end if;
         end loop;
         Body_Text.Append (To_Unbounded_String (Footer));
         Edits.File_Creations.Insert
           (New_Item => (Filepath => To_Path, Content => Body_Text));
      end Add_New_Package_Body;

      -------------------------
      -- Update_Package_Body --
      -------------------------

      procedure Update_Package_Body
        (Edits      : out Refactoring_Edits;
         From_Decls : Decl_Vector;
         To_Body    : Analysis_Unit)
      is
         Subprogram_Stub : Unbounded_String;
         Generated_Block : Unbounded_String;
         use LAL_Refactor.Stub_Utils;
      begin
         for Decl of From_Decls loop
            Subprogram_Stub :=
              Create_Code_Generator (Decl.As_Subp_Decl)
                .Generate_Subprogram_Body;
            if Subprogram_Stub = Null_Unbounded_String then
               Add_Subprogram_Diagnostic (Edits, Decl);
            else
               Append (Source => Generated_Block, New_Item => Subprogram_Stub);
            end if;
         end loop;
         Edits.Text_Edits.Insert
           (Key      => To_Body.Get_Filename,
            New_Item =>
              Text_Edit_Ordered_Sets.To_Set
                (New_Item =>
                   (Location => Get_Insertion_Point (To_Body),
                    Text     => Generated_Block)));
      end Update_Package_Body;
   begin
      if Package_Body_Exists (Self.Spec) then
         Update_Package_Body
           (Edits      => Package_Edits,
            From_Decls => Self.Subprograms,
            To_Body    => Self.Spec.P_Body_Part.Unit);
      else
         Add_New_Package_Body
           (Edits      => Package_Edits,
            From_Spec  => Self.Spec,
            With_Decls => Self.Subprograms,
            To_Path    => Self.Body_Path);
      end if;
      return Package_Edits;
   exception
      when E : others =>
         Refactor_Trace.Trace (E, "Failed to produce refactoring edits");
         --  Remove edits but keep any diagnostics
         Package_Edits.Text_Edits.Clear;
         Package_Edits.File_Creations.Clear;
         return Package_Edits;
   end Refactor;

   -------------------------------
   --  Diagnostic reporting --

   --------------
   -- Filename --
   --------------

   overriding
   function Filename (Self : Generate_Package_Problem) return String
   is (VSS.Strings.Conversions.To_UTF_8_String (Self.Filename));

   --------------
   -- Location --
   --------------

   overriding
   function Location
     (Self : Generate_Package_Problem) return Source_Location_Range
   is (Self.Location);

   ----------
   -- Info --
   ----------

   overriding
   function Info (Self : Generate_Package_Problem) return String
   is (VSS.Strings.Conversions.To_UTF_8_String (Self.Info));

   -------------
   -- Problem --
   -------------

   function Problem
     (Msg, File : String; SLOC : Source_Location_Range)
      return Generate_Package_Problem
   is (Info     => VSS.Strings.Conversions.To_Virtual_String (Msg),
       Location => SLOC,
       Filename => VSS.Strings.Conversions.To_Virtual_String (File));
end LAL_Refactor.Generate_Package;
