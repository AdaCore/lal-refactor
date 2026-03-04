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
with Langkit_Support.Slocs;
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
     (Node : Ada_Node; Spec : out Base_Package_Decl) return Boolean is
   begin
      if Node.Is_Null then
         return False;
      end if;
      Spec := To_Package_Decl (Node);
      return Can_Generate (Spec);
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

   ------------------------------
   -- Create_Package_Generator --
   ------------------------------

   function Create_Package_Generator
     (Spec : Base_Package_Decl) return Package_Generator
   is
      Public_Decls  : constant Ada_Node_Array :=
        (if Spec.F_Public_Part.Is_Null
         then []
         else Spec.F_Public_Part.F_Decls.Children);
      Private_Decls : constant Ada_Node_Array :=
        (if Spec.F_Private_Part.Is_Null
         then []
         else Spec.F_Private_Part.F_Decls.Children);
      Pkg_Decls     : constant Ada_Node_Array := Public_Decls & Private_Decls;
      --  This should be non-empty

      function To_Range (S : Source_Location) return Source_Location_Range
      is (Make_Range (S, S));
      --  Refactoring edits require a location range, but insertion
      --  without deletion typically only needs one point

      function Has_Empty_Body (Spec : Base_Package_Decl) return Boolean
      is (Package_Body_Exists (Spec)
          and then Spec.P_Body_Part.F_Decls.F_Decls.Children_Count = 0);
      --  Check for edge case when inserting into package body

      function Filter_Subp_Set
        (From  : Positive := Positive'First;
         Nodes : Ada_Node_Array;
         Match : not null access function (N : Ada_Node'Class) return Boolean;
         Til   : not null access function (N : Ada_Node'Class) return Boolean;
         Set   : in out Subp_Set) return Positive;
      --  Starting at From and iterating through each N in Nodes,
      --  add N to Set when Match (N); stop if Til (N) or the array ends.
      --  Return final iteration index. Set may be empty.

      function Build_Subp_Set (From_Decls : Ada_Node_Array) return Subp_Set
      with Post => not Build_Subp_Set'Result.Is_Empty;
      --  Build list of all subprograms to generate in new package body.
      --  Used by Generate Package action,
      --  and by Update Package in the edge case that
      --  package body object exists without any child declarations

      function Build_Singleton_Map
        (From_Decls : Ada_Node_Array) return Subp_Map;
      --  Edge case : insertion into an empty package body
      --  Produces a map with one subprogram block
      --  to be inserted above the package F_End_Name node

      function Build_Subp_Map (From_Decls : Ada_Node_Array) return Subp_Map
      with
        Pre  => Package_Body_Exists (Spec) and not Has_Empty_Body (Spec),
        Post => not Build_Subp_Map'Result.Is_Empty;
      --  Builds map of subprogram declarations without an implementation.
      --  Used by Update Package action
      --
      --  Builds a map of unimplemented subprograms in contiguous blocks
      --  separated by subprograms which already exist in the package body.
      --  Each element is a block of subprograms,
      --  and each key is the unique insertion point for this block.

      ---------------------
      -- Filter_Subp_Set --
      ---------------------

      function Filter_Subp_Set
        (From  : Positive := Positive'First;
         Nodes : Ada_Node_Array;
         Match : not null access function (N : Ada_Node'Class) return Boolean;
         Til   : not null access function (N : Ada_Node'Class) return Boolean;
         Set   : in out Subp_Set) return Positive
      is
         Idx : Positive := From;
      begin
         while Idx <= Nodes'Last loop
            exit when Til (Nodes (Idx));
            if Match (Nodes (Idx)) then
               Set.Insert (Nodes (Idx).As_Subp_Decl);
            end if;
            Idx := Positive'Succ (Idx);
         end loop;
         return Idx;
      end Filter_Subp_Set;

      --------------------
      -- Build_Subp_Set --
      --------------------

      function Build_Subp_Set (From_Decls : Ada_Node_Array) return Subp_Set is
         Set     : Subp_Set;
         End_Idx : Positive;
      begin
         End_Idx :=
           Filter_Subp_Set
             (Nodes => From_Decls,
              Match => Is_Unimplemented_Subprogram'Access,
              Til   => Libadalang.Analysis.Is_Null'Access,
              Set   => Set);
         if End_Idx < From_Decls'Last then
            raise Constraint_Error with End_Idx'Img;
         end if;
         return Set;
      end Build_Subp_Set;

      -------------------------
      -- Build_Singleton_Map --
      -------------------------

      function Build_Singleton_Map
        (From_Decls : Ada_Node_Array) return Subp_Map
      is
         Insert_Point : constant Source_Location_Range :=
           To_Range (Spec.P_Body_Part.F_Decls.Sloc_Range.End_Sloc);
      begin
         return Singleton_Map : Subp_Map do
            Singleton_Map.Insert (Insert_Point, Build_Subp_Set (From_Decls));
         end return;
      end Build_Singleton_Map;

      --------------------
      -- Build_Subp_Map --
      --------------------

      function Build_Subp_Map (From_Decls : Ada_Node_Array) return Subp_Map is

         --  Insertion point helpers
         function Insert_Below
           (N : Ada_Node'Class) return Source_Location_Range
         is (To_Range ((LAL_Refactor.Utils.Line_Below (N), 1)));

         function Insert_Above
           (N : Ada_Node'Class) return Source_Location_Range
         is (To_Range ((LAL_Refactor.Utils.Line_Above (N), 1)));

         function Is_Context_Node (N : Ada_Node'Class) return Boolean
         is (N.Kind in Ada_Subp_Decl_Range
             and then not N.As_Subp_Decl.P_Body_Part.Is_Null
             and then
               N.As_Subp_Decl.P_Body_Part.P_Parent_Basic_Decl.Kind
               in Ada_Package_Body_Range);
         --  Group unimplemented subprogram declarations
         --  around implemented subprograms that exist in the package body.
         --  These are context nodes, and will be used
         --  to calculate insertion point

         Map                    : Subp_Map;
         Subp_Batch             : Subp_Set;
         Insert_Point           : Source_Location_Range;
         Context_Idx            : Natural := 0;
         --  Store idx of last context node
         Subp_Above, Subp_Below : Subp_Decl := No_Subp_Decl;
         --  Track context nodes (implemented subprograms) declared
         --  before or after unimplemented subp decls
         Idx                    : Positive := From_Decls'First;

      begin
         while Idx <= From_Decls'Last loop
            --  To insert subprogram body stubs in the order of declaration,
            --  note the nearest implemented subprogram in the spec
            --  and insert above or below.
            if Context_Idx in From_Decls'Range then
               Subp_Above := From_Decls (Context_Idx).As_Subp_Decl;
            end if;
            --  Only need Subp_Above for first subp batch
            Context_Idx :=
              Filter_Subp_Set
                (From  => Idx,
                 Nodes => From_Decls,
                 Match => Is_Unimplemented_Subprogram'Access,
                 Til   => Is_Context_Node'Access,
                 Set   => Subp_Batch);
            if Context_Idx in From_Decls'Range then
               Subp_Below := From_Decls (Context_Idx).As_Subp_Decl;
            end if;

            if not Subp_Batch.Is_Empty then
               Insert_Point :=
                 (if Subp_Above.Is_Null
                  then Insert_Above (Subp_Below.P_Body_Part)
                  else Insert_Below (Subp_Above.P_Body_Part));
               Map.Insert (Insert_Point, Subp_Batch);
               Subp_Batch.Clear;
            end if;
            Idx := Positive'Succ (Context_Idx);
         end loop;
         return Map;
      end Build_Subp_Map;

   begin
      return
        (if Package_Body_Exists (Spec)
         then
           (Action          => Update_Existing,
            Spec            => Spec,
            Body_Path       => To_Unbounded_String (Get_Body_Path (Spec)),
            New_Subprograms =>
              (if Has_Empty_Body (Spec)
               then Build_Singleton_Map (Pkg_Decls)
               else Build_Subp_Map (Pkg_Decls)))
         else
           (Action          => Create_New,
            Spec            => Spec,
            Body_Path       => To_Unbounded_String (Get_Body_Path (Spec)),
            All_Subprograms => Build_Subp_Set (Pkg_Decls)));
   end Create_Package_Generator;

   --------------
   -- Refactor --
   --------------

   overriding
   function Refactor
     (Self           : Package_Generator;
      Analysis_Units : access function return Analysis_Unit_Array)
      return Refactoring_Edits
   is
      procedure Add_New_Package_Edits
        (Edits      : out Refactoring_Edits;
         From_Spec  : Base_Package_Decl;
         With_Decls : Subp_Set)
      with Pre => not (From_Spec.Is_Null or With_Decls.Is_Empty);
      --  Create edits to go into one new file

      procedure Update_Package_Edits
        (Edits : out Refactoring_Edits; New_Decls : Subp_Map)
      with Pre => not New_Decls.Is_Empty;
      --  Text edits to modify an existing package body

      ---------------------------
      -- Add_New_Package_Edits --
      ---------------------------

      procedure Add_New_Package_Edits
        (Edits      : out Refactoring_Edits;
         From_Spec  : Base_Package_Decl;
         With_Decls : Subp_Set)
      is
         Package_Body_Text : constant Unbounded_String :=
           LAL_Refactor.Stub_Utils.Create_Code_Generator
             (From_Spec, With_Decls)
             .Generate_Body;
      begin
         Edits.File_Creations.Insert ((Self.Body_Path, Package_Body_Text));
      end Add_New_Package_Edits;

      --------------------------
      -- Update_Package_Edits --
      --------------------------

      procedure Update_Package_Edits
        (Edits : out Refactoring_Edits; New_Decls : Subp_Map)
      is
         --  Iterate over subprogram blocks and convert to edit
         procedure Insert_Edit (Position : Subp_Decl_Maps.Cursor)
         with Pre => Position.Has_Element;

         -----------------
         -- Insert_Edit --
         -----------------

         procedure Insert_Edit (Position : Subp_Decl_Maps.Cursor) is
            use LAL_Refactor.Stub_Utils;

            Subp_Batch : constant Subp_Set := Position.Element;
            Insert_Loc : constant Source_Location_Range := Position.Key;

            Package_Stub : Unbounded_String;

            procedure Generate_Subp_Stub (Position : Subp_Sets.Cursor)
            with Pre => Position.Has_Element;
            --  Generate individual subprogram stub

            ------------------------
            -- Generate_Subp_Stub --
            ------------------------

            procedure Generate_Subp_Stub (Position : Subp_Sets.Cursor) is
               Subprogram_Decl : constant Subp_Decl := Position.Element;
               Subprogram_Stub : constant Unbounded_String :=
                 Create_Code_Generator (Subprogram_Decl).Generate_Body;
            begin
               Package_Stub.Append (Subprogram_Stub);
            end Generate_Subp_Stub;
         begin
            if not Subp_Batch.Is_Empty then
               Subp_Batch.Iterate (Generate_Subp_Stub'Access);
               Safe_Insert
                 (Edits     => Edits.Text_Edits,
                  File_Name => Self.Body_Path.To_String,
                  Edit      => (Insert_Loc, Package_Stub));
            end if;
         end Insert_Edit;

      begin
         if not New_Decls.Is_Empty then
            New_Decls.Iterate (Insert_Edit'Access);
         end if;
      end Update_Package_Edits;

      Package_Edits : Refactoring_Edits := No_Refactoring_Edits;
   begin
      case Self.Action is
         when Update_Existing =>
            Update_Package_Edits
              (Edits     => Package_Edits,
               New_Decls => Self.New_Subprograms);

         when Create_New      =>
            Add_New_Package_Edits
              (Edits      => Package_Edits,
               From_Spec  => Self.Spec,
               With_Decls => Self.All_Subprograms);
      end case;
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
