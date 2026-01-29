--
--
--  Copyright (C) 2025-2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  This package contains refactoring tools to generate or update
--  a GNAT-compliant package body file from a package declaration

with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with VSS.Strings.Conversions;

with Libadalang.Common;    use Libadalang.Common;
with Langkit_Support.Text; use Langkit_Support.Text;

package body LAL_Refactor.Generate_Package is
   Tool_Name : constant String := "Generate Package";

   ---------------------
   -- To_Package_Decl --
   ---------------------

   function To_Package_Decl (Node : Ada_Node) return Base_Package_Decl is
      Base_Pkg : Base_Package_Decl := No_Base_Package_Decl;

      procedure Set_Parent_Package
        (Node : Ada_Node; Spec : out Base_Package_Decl);
      --  Traverse Full.Package.Name up to parent package node

      procedure Set_Parent_Package
        (Node : Ada_Node; Spec : out Base_Package_Decl)
      is
         function Inside_Name (Node : Ada_Node) return Boolean
         is (Node.Kind
             in Ada_Identifier_Range
              | Ada_End_Name_Range
              | Ada_Defining_Name_Range
              | Ada_Dotted_Name_Range);

         N : Ada_Node := Node;
      begin
         while Inside_Name (N) and then not N.Parent.Is_Null loop
            N := N.Parent;
         end loop;
         Spec :=
           (if N.Kind in Ada_Base_Package_Decl
            then N.As_Base_Package_Decl
            else No_Base_Package_Decl);
      end Set_Parent_Package;
   begin
      if Node.Is_Null then
         return No_Base_Package_Decl;
      elsif Node.Kind in Ada_Base_Package_Decl then
         Base_Pkg := Node.As_Base_Package_Decl;
      elsif Node.Kind in Ada_Generic_Package_Decl_Range then
         Base_Pkg :=
           Node.As_Generic_Package_Decl.F_Package_Decl.As_Base_Package_Decl;
      else
         Set_Parent_Package (Node, Base_Pkg);
      end if;
      return Base_Pkg;
   end To_Package_Decl;

   -----------------------------------
   -- Is_Generate_Package_Available --
   -----------------------------------

   function Is_Generate_Package_Available
     (Node : Ada_Node; Spec : out Base_Package_Decl) return Boolean
   is
      function Has_Unimplemented_Subprograms
        (Decl_Block : Declarative_Part'Class) return Boolean
      is (not Decl_Block.Is_Null
          and then
            (for some Decl of Decl_Block.F_Decls =>
               Is_Unimplemented_Subprogram (Decl)));
   begin
      Spec := To_Package_Decl (Node);
      return
        (not Spec.Is_Null
         and then
           (Has_Unimplemented_Subprograms (Spec.F_Public_Part)
            or Has_Unimplemented_Subprograms (Spec.F_Private_Part)));
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
   begin
      if Package_Body_Exists (From_Spec) then
         return From_Spec.P_Body_Part.Unit.Get_Filename;
      else
         --  Create a new file in the same directory
         --  No guarantee that filenaming conventions follow GNAT,
         --  but Generate Package will use a GNAT-compliant naming scheme
         --  and create a body file with an ".adb" suffix,
         --  even if the spec does not use ".ads"

         declare
            S   : constant String := From_Spec.Unit.Get_Filename;
            Sfx : constant String := ".adb";
            Idx : constant Natural :=
              Ada.Strings.Fixed.Index
                (Source => S,
                 Set    => Ada.Strings.Maps.To_Set ("."),
                 From   => S'Last,
                 Test   => Ada.Strings.Inside,
                 Going  => Ada.Strings.Backward);
         begin
            --  Unlikely that the file will have no extension
            --  but it's a simple fix
            if Idx = 0 then
               return S & Sfx;

            else
               return S (S'First .. Idx - 1) & Sfx;
            end if;
         end;
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
      begin
         return Decls : Decl_Vector do
            if not Spec.F_Public_Part.Is_Null then
               for Decl of Spec.F_Public_Part.F_Decls loop
                  if Is_Unimplemented_Subprogram (Decl) then
                     Decls.Append (Decl.As_Subp_Decl);
                  end if;
               end loop;
            end if;
            if not Spec.F_Private_Part.Is_Null then
               for Decl of Spec.F_Private_Part.F_Decls loop
                  if Is_Unimplemented_Subprogram (Decl) then
                     Decls.Append (Decl.As_Subp_Decl);
                  end if;
               end loop;
            end if;
         end return;
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

      function Get_Insertion_Point
        (Body_Unit : Analysis_Unit := No_Analysis_Unit)
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

      procedure Add_New_Package_Edits
        (Edits      : out Refactoring_Edits;
         From_Spec  : Base_Package_Decl;
         With_Decls : Decl_Vector;
         To_Path    : Unbounded_String);
      --  Create edits to go into one new file

      procedure Update_Package_Edits
        (Edits      : out Refactoring_Edits;
         From_Decls : Decl_Vector;
         To_Body    : Analysis_Unit);
      --  Text edits to modify an existing package body

      -------------------------
      -- Get_Insertion_Point --
      -------------------------

      function Get_Insertion_Point
        (Body_Unit : Analysis_Unit := No_Analysis_Unit)
         return Source_Location_Range
      is
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

      ---------------------------
      -- Add_New_Package_Edits --
      ---------------------------

      procedure Add_New_Package_Edits
        (Edits      : out Refactoring_Edits;
         From_Spec  : Base_Package_Decl;
         With_Decls : Decl_Vector;
         To_Path    : Unbounded_String)
      is
         Package_Body_Text : constant Unbounded_String :=
           LAL_Refactor.Stub_Utils.Create_Code_Generator
             (Spec => From_Spec, Subprograms => With_Decls)
             .Generate_Body;
      begin
         Edits.File_Creations.Insert
           (New_Item => (Filepath => To_Path, Content => Package_Body_Text));
      end Add_New_Package_Edits;

      --------------------------
      -- Update_Package_Edits --
      --------------------------

      procedure Update_Package_Edits
        (Edits      : out Refactoring_Edits;
         From_Decls : Decl_Vector;
         To_Body    : Analysis_Unit)
      is
         Subprogram_Stub : Unbounded_String;
         Generated_Block : Unbounded_String;
         use LAL_Refactor.Stub_Utils;
      begin
         for Decl of From_Decls loop
            Subprogram_Stub := Create_Code_Generator (Decl).Generate_Body;
            Append (Source => Generated_Block, New_Item => Subprogram_Stub);
         end loop;
         Edits.Text_Edits.Insert
           (Key      => To_Body.Get_Filename,
            New_Item =>
              Text_Edit_Ordered_Sets.To_Set
                (New_Item =>
                   (Location => Get_Insertion_Point (To_Body),
                    Text     => Generated_Block)));
      end Update_Package_Edits;
   begin
      if Package_Body_Exists (Self.Spec) then
         Update_Package_Edits
           (Edits      => Package_Edits,
            From_Decls => Self.Subprograms,
            To_Body    => Self.Spec.P_Body_Part.Unit);
      else
         Add_New_Package_Edits
           (Edits      => Package_Edits,
            From_Spec  => Self.Spec,
            With_Decls => Self.Subprograms,
            To_Path    => Self.Body_Path);
      end if;
      return Package_Edits;
   exception
      when E : others =>
         Refactor_Trace.Trace (E, "Failed to produce refactoring edits");
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
