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
with LAL_Refactor.Utils;
with VSS.Strings.Conversions;

with Langkit_Support.Text; use Langkit_Support.Text;

package body LAL_Refactor.Generate_Package is
   Tool_Name : constant String := "Generate Package";

   ---------------------
   -- To_Package_Decl --
   ---------------------

   function To_Package_Decl (Node : Ada_Node) return Base_Package_Decl
   is (case Node.Kind is
         when Ada_Base_Package_Decl
         => Node.As_Base_Package_Decl,
         when Ada_Generic_Package_Decl_Range
         => Node.As_Generic_Package_Decl.F_Package_Decl.As_Base_Package_Decl,
         when Ada_Dotted_Name_Range | Ada_End_Name_Range | Ada_Identifier_Range
         =>
           (if not Node.P_Semantic_Parent.Is_Null
              and then Node.P_Semantic_Parent.Kind in Ada_Base_Package_Decl
            then Node.P_Semantic_Parent.As_Base_Package_Decl
            else No_Base_Package_Decl),
         when others
         => No_Base_Package_Decl);
   --  Get package node from "package [Name] is" or "end [Name]" text

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
      if Node.Is_Null then
         return False;
      end if;
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
     (Edits     : out Refactoring_Edits;
      New_Decls : Decl_Vector;
      To_Body   : Package_Body)
   is
      function Find_Insertion_Point
        (To_Body : Package_Body) return Source_Location_Range;
      --  Find a place to insert subprogram body stubs into existing
      --  package body file.
      --
      --  By default, insert just below the last declaration:
      --
      --  F_Package_Name  | package body Name
      --  F_Decls.F_Decls |   [declarations]
      --  F_Decls         |   [remaining whitespace]
      --  F_End_Name      | end Name
      --
      --  TODO : contextual insertion

      --------------------------
      -- Find_Insertion_Point --
      --------------------------

      function Find_Insertion_Point
        (To_Body : Package_Body) return Source_Location_Range
      is
         Body_Decls   : constant Ada_Node_List := To_Body.F_Decls.F_Decls;
         End_Line     : constant Line_Number :=
           To_Body.F_End_Name.Sloc_Range.Start_Line;
         Insert_Point : Source_Location := (End_Line, Column_Number (1));
      begin
         if not (Body_Decls.Is_Null or else Body_Decls.Last_Child.Is_Null) then
            Insert_Point.Line :=
              LAL_Refactor.Utils.Expand_SLOC_To_Docstring
                (Body_Decls.Last_Child)
                .End_Line
              + Line_Number (1);
         end if;
         return Make_Range (Insert_Point, Insert_Point);
      end Find_Insertion_Point;

      Package_Stubs : Unbounded_String;
      use LAL_Refactor.Stub_Utils;
   begin
      for Decl of New_Decls loop
         Package_Stubs.Append (Create_Code_Generator (Decl).Generate_Body);
      end loop;
      Safe_Insert
        (Edits.Text_Edits,
         To_Body.Unit.Get_Filename,
         (Location => Find_Insertion_Point (To_Body), Text => Package_Stubs));
   end Update_Package_Edits;

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
   begin
      if Package_Body_Exists (Self.Spec) then
         Update_Package_Edits
           (Edits     => Package_Edits,
            New_Decls => Self.Subprograms,
            To_Body   => Self.Spec.P_Body_Part);
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
