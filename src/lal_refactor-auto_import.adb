--
--  Copyright (C) 2023-2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Characters.Latin_1;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Vectors;
with Ada.Strings.Wide_Wide_Unbounded;

with Langkit_Support.Errors;

with Libadalang.Common; use Libadalang.Common;
with GNATCOLL.Traces;   use GNATCOLL.Traces;

package body LAL_Refactor.Auto_Import is

   Me : constant Trace_Handle := Create ("LAL_REFACTOR.AUTO_IMPORT", Off);

   package Basic_Decl_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Basic_Decl,
      "="          => "=");

   subtype Basic_Decl_Vector is Basic_Decl_Vectors.Vector;

   package Defining_Name_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Defining_Name,
      "="          => "=");

   subtype Defining_Name_Vector is Defining_Name_Vectors.Vector;

   function Defining_Name_Hash
     (Defining_Name : Libadalang.Analysis.Defining_Name)
      return Ada.Containers.Hash_Type
   is (Hash (Defining_Name.As_Ada_Node));
   --  Casts Basic_Decl as Ada_Node and use Hash from Libadalang.Analysis

   package Defining_Name_Hashed_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type        => Defining_Name,
      Hash                => Defining_Name_Hash,
      Equivalent_Elements => "=",
      "="                 => "=");

   subtype Defining_Name_Hashed_Set is Defining_Name_Hashed_Sets.Set;

   package Defining_Name_To_Defining_Name_Vector_Hashed_Maps is new
     Ada.Containers.Hashed_Maps
       (Key_Type        => Defining_Name,
        Element_Type    => Defining_Name_Vector,
        Hash            => Defining_Name_Hash,
        Equivalent_Keys => "=",
        "="             => Defining_Name_Vectors."=");

   subtype Defining_Name_To_Defining_Name_Vector_Hashed_Map is
     Defining_Name_To_Defining_Name_Vector_Hashed_Maps.Map;

   procedure Append_Reachable_Declarations
     (Name                  : Libadalang.Analysis.Name;
      Unit                  : Compilation_Unit;
      Reachable_Definitions : in out Defining_Name_Hashed_Set;
      Reachable_Renames     :
        in out Defining_Name_To_Defining_Name_Vector_Hashed_Map);
   --  Finds all declarations on Unit that can be visible by Name with the
   --  with clause or qualifier. These are stored in Reachable_Declarations.
   --  Additionally, also updates Reachable_Renames with all rename
   --  declarations seen.

   function Create_Available_Imports
     (Name_To_Import         : Name;
      Reachable_Declarations : Defining_Name_Hashed_Set;
      Reachable_Renames      :
        Defining_Name_To_Defining_Name_Vector_Hashed_Map)
      return Import_Type_Ordered_Set;
   --  For every declaration of Reachable_Declarations, creates at least one
   --  Import_Type object that is added to the returned set.
   --  While creating the Import_Type, if there is any alias that can be used
   --  in Reachable_Renames, a new Import_Type is created and also added
   --  to the returned set.
   --
   --  Example:
   --  Given Reachable_Declarations = [A, A.B] and
   --  Reachable_Renames = {A: [C], B: [D]}, the following imports will be
   --  generated:
   --  A
   --  C (an alias of A)
   --  A.B
   --  A.D (an alias of D)
   --  C.B (an alias of A)
   --  C.D (an alias of A and an alias of D)
   --
   --  Name_To_Import is used to compare the original qualifier (which only
   --  exits if its parent is a Dotted_Name) with the one that that needs to be
   --  added. If they're the same, it means that it does not need to be added
   --  (only imported), therefore, the Import_Type object added to the set will
   --  contain an empty qualifier.

   function Get_Appropriate_Enclosing_Name
     (Base_Id : Libadalang.Analysis.Base_Id)
      return Name;
   --  Returns the top dotted name parent suffix if Base_Id is part of one,
   --  or Base_Id casted as Name if it is not.

   function Get_Available_Imports
     (Name  : Libadalang.Analysis.Name;
      Units : Analysis_Unit_Array)
      return Import_Type_Ordered_Set;
   --  Returns an ordered set with all the with clause and qualifiers that make
   --  Name resolvable.

   function Get_Generic_Package_Internal
     (Instantiation : Generic_Package_Instantiation)
      return Generic_Package_Internal;
   --  Finds the Generic_Package_Internal node given a
   --  Generic_Package_Instantiation. Note that P_Designated_Generic_Decl
   --  sometimes raises an unexpected exception when Gen_Pkg_Instantiation is
   --  a node from an Ada runtime file. At the moment, this is considered
   --  a LAL bug: [T814-031]

   function Get_Insert_New_Dependency_Location
     (Unit           : Compilation_Unit'Class;
      New_Dependency : Langkit_Support.Text.Text_Type)
      return Source_Location;
   --  Source_Location where to insert a with clause for New_Dependency.
   --
   --  The insert location is determined assuming that the prelude (ignoring
   --  pragmas) is already sorted alphabetically and New_Dependency will be
   --  inserted accordingly. In this case, the returned Source_Location will
   --  always be the Start_Sloc of an existing clause. This implies that if the
   --  with clause is a list and if any package is alphabetically greater than
   --  New_Dependency, then this function returns the Start_Sloc of that with
   --  clause.
   --
   --  Examples, considering New_Dependency = "B":
   --
   --  with A;
   --  with C;
   --  ^
   --
   --  with A, C;
   --  ^
   --  with D;
   --
   --  ^ represents the Source_Location returned by this function.
   --
   --  If Unit does not have a prelude, or if it is empty, then returns the
   --  Start_Sloc of Unit's body to ensure that New_Dependency is inserted
   --  after any comments like copyright headers.

   function Get_Parent_Packages
     (Defining_Name : Libadalang.Analysis.Defining_Name'Class)
      return Basic_Decl_Vector;
   --  Returns a vector with all the parent package needed to fully qualify
   --  Node.

   function List_And_Expand_Package_Declarations
     (Package_Decl    : Base_Package_Decl'Class;
      Include_Private : Boolean)
      return Basic_Decl_Vector;
   --  Add every declaration of Package_Decl to a list. If any declaration is
   --  also a Base_Package_Decl (in case of nested packages) then the nested
   --  package is also expanded and its declarations are added to the same
   --  list. Include_Private controls whether or not private declarations are
   --  also added.

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Import_Type) return Boolean
   is
      use Ada.Strings.Wide_Wide_Unbounded;

   begin
      return
        (if Left.Import = Right.Import then
            Left.Qualifier < Right.Qualifier
         else
            Left.Import < Right.Import);
   end "<";

   -----------------------------------
   -- Append_Reachable_Declarations --
   -----------------------------------

   procedure Append_Reachable_Declarations
     (Name                  : Libadalang.Analysis.Name;
      Unit                  : Compilation_Unit;
      Reachable_Definitions : in out Defining_Name_Hashed_Set;
      Reachable_Renames     :
        in out Defining_Name_To_Defining_Name_Vector_Hashed_Map)
   is
      Name_Text : constant Langkit_Support.Text.Text_Type :=
        Langkit_Support.Text.To_Lower (Name.Text);

      procedure Append_Declaration
        (Declaration     : Basic_Decl;
         Include_Private : Boolean);
      --  Appends the canonical part of all definitions in Declaration to
      --  Reachable_Definitions if and only if the definition has the name
      --  text as Name. If Declaration is a Package_Renaming_Decl, then it's
      --  also appended to Reachable_Renames.
      --  If Declaration is a package declaration, then the package is also
      --  visited and this function is recursively called on all its
      --  declarations.
      --  Include_Private controls if declarations inside the private part of
      --  a package are considered or not.

      procedure Process_Own_Unit;

      ------------------------
      -- Append_Declaration --
      ------------------------

      procedure Append_Declaration
        (Declaration     : Basic_Decl;
         Include_Private : Boolean)
      is
         procedure Append (Declaration : Basic_Decl);
         --  Appends the canonical part of all definitions in Declaration to
         --  Reachable_Definitions if and only if the definition has the name
         --  text as Name.

         procedure Append_Rename (Package_Renaming : Package_Renaming_Decl);
         --  If Reachable_Renames already has a key equal to Package_Renaming
         --  then its set is expanded with Declaration, otherwise and new set
         --  is created with Declaration as the only element.

         ------------
         -- Append --
         ------------

         procedure Append (Declaration : Basic_Decl) is
         begin
            if Declaration.Is_Null then
               return;
            end if;

            for Defining_Name of Declaration.P_Defining_Names loop
               if Defining_Name.F_Name.Kind in Ada_Dotted_Name then
                  declare
                     Text : constant Langkit_Support.Text.Text_Type :=
                       Langkit_Support.Text.To_Lower
                         (Defining_Name.F_Name.As_Dotted_Name.F_Suffix.Text);

                  begin
                     if Text = Name_Text then
                        Reachable_Definitions.Include
                          (Defining_Name.P_Canonical_Part);
                     end if;

                  end;

               else
                  declare
                     Text : constant Langkit_Support.Text.Text_Type :=
                       Langkit_Support.Text.To_Lower
                         (Defining_Name.F_Name.Text);

                  begin
                     if Text = Name_Text then
                        Reachable_Definitions.Include
                          (Defining_Name.P_Canonical_Part);
                     end if;
                  end;
               end if;
            end loop;
         end Append;

         -------------------
         -- Append_Rename --
         -------------------

         procedure Append_Rename (Package_Renaming : Package_Renaming_Decl) is
            Renamed_Definition : constant Defining_Name :=
              (declare
                 Renamed_Package : constant Basic_Decl :=
                   Package_Renaming.P_Renamed_Package;
               begin
                 (if Renamed_Package.Is_Null then No_Defining_Name
                  else Renamed_Package.P_Defining_Name));

         begin
            if Renamed_Definition.Is_Null then
               LAL_Refactor.Refactor_Trace.Trace
                 ("Renamed package of "
                  & Package_Renaming.Image
                  & " has an unexpected null definition");

               return;
            end if;

            if Reachable_Renames.Contains (Renamed_Definition) then
               Reachable_Renames
                 .Reference (Renamed_Definition)
                 .Append (Package_Renaming.P_Defining_Name);
            else
               Reachable_Renames
                 .Insert
                    (Renamed_Definition, [Package_Renaming.P_Defining_Name]);
            end if;
         end Append_Rename;

      begin
         if Declaration.Kind in Ada_Error_Decl then
            null;

         elsif Declaration.Kind = Ada_Package_Decl then
            Append (Declaration.As_Basic_Decl);

            --  Nodes of type Package_Decl are nested packages and need to be
            --  expanded since they contain more declarations

            for Inner_Declaration of
              List_And_Expand_Package_Declarations
                (Declaration.As_Package_Decl,
                 Include_Private)
            loop
               Append (Inner_Declaration);

               if Inner_Declaration.Kind in
                 Ada_Package_Renaming_Decl_Range
               then
                  Append_Rename (Inner_Declaration.As_Package_Renaming_Decl);
               end if;
            end loop;

         elsif Declaration.Kind = Ada_Generic_Package_Instantiation then
            Append (Declaration.As_Basic_Decl);

            for Inner_Declaration of List_And_Expand_Package_Declarations
              (Get_Generic_Package_Internal
                 (Declaration.As_Generic_Package_Instantiation),
               False)
            loop
               Append (Inner_Declaration);

               if Inner_Declaration.Kind in
                 Ada_Package_Renaming_Decl_Range
               then
                  Append_Rename (Inner_Declaration.As_Package_Renaming_Decl);
               end if;
            end loop;

         else
            Append (Declaration.As_Basic_Decl);

            if Declaration.Kind in Ada_Package_Renaming_Decl_Range then
               Append_Rename (Declaration.As_Package_Renaming_Decl);
               --  ??? The same code should be applicable to
               --  Ada_Generic_Renaming_Decl and
               --  Ada_Subp_Renaming_Decl_Range. However, for now, LAL does
               --  not provide property P_Renamed_Package for such types.
            end if;
         end if;
      end Append_Declaration;

      ----------------------
      -- Process_Own_Unit --
      ----------------------

      procedure Process_Own_Unit is
         Stop_Decl : Basic_Decl := No_Basic_Decl;

      begin
         for Parent of Name.Parent.Parents loop
            if Parent.Kind in Ada_Basic_Decl
              and then Stop_Decl.Is_Null
            then
               Stop_Decl := Parent.As_Basic_Decl;
            end if;

            if Parent.Kind = Ada_Handled_Stmts then
               --  If this Handled_Statements has a corresponding
               --  Declarative_Part, add all those declarations to Map.
               --  If a declaration is a package declaration, then only add
               --  the visible declarations.

               declare
                  Declarative_Part :
                    constant Libadalang.Analysis.Declarative_Part :=
                      Laltools.Common.Get_Declarative_Part
                        (Parent.As_Handled_Stmts);
               begin
                  if not Declarative_Part.Is_Null then
                     for Declaration of Declarative_Part.F_Decls loop
                        if Declaration.Kind in Ada_Basic_Decl then
                           Append_Declaration
                             (Declaration     => Declaration.As_Basic_Decl,
                              Include_Private => False);
                        end if;
                     end loop;
                  end if;
               end;

            elsif Parent.Kind = Ada_Declarative_Part then
               declare
                  Declarative_Part :
                    constant Libadalang.Analysis.Declarative_Part :=
                      Parent.As_Declarative_Part;

               begin
                  if not Declarative_Part.Is_Null then
                     --  For a node in a Declarative_Part of a Package_Body
                     --  only the previously declared declarations are
                     --  visible.

                     Decl_Loop :
                     for Declaration of Declarative_Part.F_Decls loop
                        --  Allow recursion for declarations that are not
                        --  packages

                        if Declaration.Kind in Ada_Basic_Decl
                          and then Declaration.As_Basic_Decl = Stop_Decl
                        then
                           if not (Declaration.Kind in
                                     Ada_Package_Decl_Range
                                     | Ada_Package_Body_Range)
                           then
                              Append_Declaration
                                (Declaration     =>
                                   Declaration.As_Basic_Decl,
                                 Include_Private => False);
                           end if;

                           exit Decl_Loop;
                        end if;

                        --  Do not add Package_Body

                        if Declaration.Kind in Ada_Basic_Decl
                          and then Declaration.Kind not in
                            Ada_Package_Body_Range
                        then
                           Append_Declaration
                             (Declaration         =>
                                Declaration.As_Basic_Decl,
                              Include_Private     => False);
                        end if;
                     end loop Decl_Loop;
                  end if;
               end;

            elsif Parent.Kind in Ada_Package_Body_Range then
               --  If Node is inside a Package_Body then all private
               --  declarations found in the Package_Declaration are
               --  added to Map.

               if not Parent
                        .As_Basic_Decl
                        .P_Defining_Name
                        .P_Canonical_Part
                        .P_Basic_Decl.Is_Null
               then
                  Append_Declaration
                    (Declaration     =>
                       Parent
                         .As_Basic_Decl
                         .P_Defining_Name.P_Canonical_Part
                         .P_Basic_Decl,
                     Include_Private => True);
               end if;
            end if;
         end loop;
      end Process_Own_Unit;

   begin
      if Name.P_Enclosing_Compilation_Unit = Unit then
         Process_Own_Unit;

      else
         Append_Declaration
           (Declaration     => Unit.P_Decl,
            Include_Private =>
              Name.P_Enclosing_Compilation_Unit = Unit.P_Other_Part);
      end if;
   end Append_Reachable_Declarations;

   --------------------------
   -- Create_Auto_Importer --
   --------------------------

   function Create_Auto_Importer
     (Unit     : Analysis_Unit;
      Location : Source_Location;
      Import   : Import_Type)
      return Auto_Importer
   is
      Node           : constant Ada_Node := Unit.Root.Lookup (Location);
      Enclosing_Name : constant Name :=
        Get_Appropriate_Enclosing_Name (Node.As_Base_Id);

   begin
      return Create_Auto_Importer (Enclosing_Name, Import);
   end Create_Auto_Importer;

   --------------------------
   -- Create_Auto_Importer --
   --------------------------

   function Create_Auto_Importer
     (Name   : Libadalang.Analysis.Name;
      Import : Import_Type)
      return Auto_Importer
   is (Auto_Importer'(Name, Import));
   pragma Inline (Create_Auto_Importer);

   ------------------------------
   -- Create_Available_Imports --
   ------------------------------

   function Create_Available_Imports
     (Name_To_Import         : Name;
      Reachable_Declarations : Defining_Name_Hashed_Set;
      Reachable_Renames      :
        Defining_Name_To_Defining_Name_Vector_Hashed_Map)
     return Import_Type_Ordered_Set
   is
      use Ada.Strings.Wide_Wide_Unbounded;
      use Langkit_Support.Text;

      Original_Qualifier : constant Unbounded_Text_Type :=
         (if Name_To_Import.Parent.Kind in Ada_Dotted_Name then
            To_Unbounded_Text
              (Name_To_Import.Parent.As_Dotted_Name.F_Prefix.Text)
          else
            Null_Unbounded_Wide_Wide_String);

      function Is_Top_Level_Declaration
        (Defining_Name : Libadalang.Analysis.Defining_Name)
         return Boolean
      is (Defining_Name.P_Parent_Basic_Decl =
            Defining_Name.P_Enclosing_Compilation_Unit.P_Decl);
      --  True is Declaration is the top level declaration of it's enclosing
      --  compilation unit.

      function Make_Imports
        (Defining_Name : Libadalang.Analysis.Defining_Name)
         return Import_Type_Ordered_Set;
      --  Computes the package name that needs to be withed and the qualifier
      --  that needs to be used in order to resolve Definition. Multiple
      --  solutions can be found if Definition's parent packages have renames.

      ------------------
      -- Make_Imports --
      ------------------

      function Make_Imports
        (Defining_Name : Libadalang.Analysis.Defining_Name)
         return Import_Type_Ordered_Set
      is
         Parent_Packages : constant Basic_Decl_Vector :=
           Get_Parent_Packages (Defining_Name);

         Final_Imports : Import_Type_Ordered_Set :=
           [Import_Type'
              (Import    => Null_Unbounded_Wide_Wide_String,
               Qualifier => Null_Unbounded_Wide_Wide_String)];

         procedure Update_Imports
           (Imports          : in out Import_Type_Ordered_Set;
            Parent_Qualifier : Unbounded_Text_Type;
            Last_Parent      : Boolean);
         --  Adds Parent_Qualifier as a qualifier to every Import_Type object
         --  of Final_Imports. If Last_Parent is True, then adds
         --  Parent_Qualifier as the unit to import too. These new Import_Type
         --  objects are added to Imports.

         --------------------
         -- Update_Imports --
         --------------------

         procedure Update_Imports
           (Imports          : in out Import_Type_Ordered_Set;
            Parent_Qualifier : Unbounded_Text_Type;
            Last_Parent      : Boolean)
         is
         begin
            for Import of Final_Imports loop
               declare
                  New_Import : Import_Type :=
                    ((if Last_Parent
                      then Parent_Qualifier
                      else Null_Unbounded_Wide_Wide_String),
                     (if Langkit_Support.Text."="
                           (Import.Qualifier,
                            Null_Unbounded_Wide_Wide_String)
                      then Parent_Qualifier
                      else Parent_Qualifier & "." & Import.Qualifier));

               begin
                  if Langkit_Support.Text."="
                       (New_Import.Qualifier, Original_Qualifier)
                  then
                     New_Import.Qualifier :=
                       Null_Unbounded_Wide_Wide_String;
                  end if;
                  Imports.Include (New_Import);
               end;
            end loop;
         end Update_Imports;

      begin
         if Is_Top_Level_Declaration (Defining_Name) then
            declare
               Updated_Imports  : Import_Type_Ordered_Set := [];
               Declaration_Name : constant Unbounded_Text_Type :=
                 To_Unbounded_Text (Defining_Name.Text);

            begin
               declare
                  New_Import : Import_Type :=
                    (Declaration_Name, Declaration_Name);
               begin
                  Updated_Imports.Include (New_Import);
                  if Langkit_Support.Text."="
                     (New_Import.Qualifier, Original_Qualifier)
                  then
                     New_Import.Qualifier :=
                       Null_Unbounded_Wide_Wide_String;
                  end if;
                  if Reachable_Renames.Contains (Defining_Name) then
                     for Alias of
                       Reachable_Renames.Constant_Reference (Defining_Name)
                     loop
                        declare
                           Alias_Name : constant Unbounded_Text_Type :=
                             To_Unbounded_Text (Alias.Text);

                        begin
                           Update_Imports
                             (Updated_Imports,
                              Alias_Name,
                              True);
                        end;
                     end loop;
                  end if;
               end;
               Final_Imports := Updated_Imports;
            end;
            return Final_Imports;
         end if;

         for Parent of Parent_Packages loop
            declare
               Updated_Imports  : Import_Type_Ordered_Set := [];
               Parent_Qualifier : constant Unbounded_Text_Type :=
                 To_Unbounded_Text (Parent.P_Defining_Name.Text);

            begin
               Update_Imports
                 (Updated_Imports,
                  Parent_Qualifier,
                  Parent = Parent_Packages.Last_Element);
               if Reachable_Renames.Contains (Parent.P_Defining_Name) then
                  for Alias of
                    Reachable_Renames.Constant_Reference
                      (Parent.P_Defining_Name)
                  loop
                     declare
                        Alias_Name : constant Unbounded_Text_Type :=
                          To_Unbounded_Text (Alias.Text);
                     begin
                        Update_Imports
                          (Updated_Imports,
                           Alias_Name,
                           Parent = Parent_Packages.Last_Element);
                     end;
                  end loop;
               end if;
               Final_Imports := Updated_Imports;
            end;
         end loop;

         return Final_Imports;
      end Make_Imports;

      Result : Import_Type_Ordered_Set;

   begin
      for Reachable_Declaration of Reachable_Declarations loop
         Result.Union (Make_Imports (Reachable_Declaration));
      end loop;

      return Result;
   end Create_Available_Imports;

   ------------------------------------
   -- Get_Appropriate_Enclosing_Name --
   ------------------------------------

   function Get_Appropriate_Enclosing_Name
     (Base_Id : Libadalang.Analysis.Base_Id)
      return Name
   is
      Aux : Ada_Node := Base_Id.As_Ada_Node;

   begin
      while Aux.Parent.Kind in Ada_Dotted_Name loop
         Aux := Aux.Parent;
      end loop;

      if Aux.Kind in Ada_Dotted_Name then
         return Aux.As_Dotted_Name.F_Suffix.As_Name;
      else
         return Base_Id.As_Name;
      end if;

   end Get_Appropriate_Enclosing_Name;

   ---------------------------
   -- Get_Available_Imports --
   ---------------------------

   function Get_Available_Imports
     (Name  : Libadalang.Analysis.Name;
      Units : Analysis_Unit_Array)
      return Import_Type_Ordered_Set
   is
      use Ada.Strings.Wide_Wide_Unbounded;

      Reachable_Declarations : Defining_Name_Hashed_Set;
      Reachable_Renames      :
        Defining_Name_To_Defining_Name_Vector_Hashed_Map;
      Original_Qualifier_Text : constant
        Langkit_Support.Text.Text_Type :=
          (if not Name.Parent.Is_Null
           and then Name.Parent.Kind in Ada_Dotted_Name
           then
              Name.Parent.As_Dotted_Name.F_Prefix.Text
           else
              "");
      Lowered_Original_Qualifier : constant
        Langkit_Support.Text.Unbounded_Text_Type :=
          Langkit_Support.Text.To_Unbounded_Text
            (Langkit_Support.Text.To_Lower (Original_Qualifier_Text));
      Original_Qualifier : constant
        Langkit_Support.Text.Unbounded_Text_Type :=
          Langkit_Support.Text.To_Unbounded_Text
            (Original_Qualifier_Text);

      Available_Imports : Import_Type_Ordered_Set;
      Stop_Iteration : Boolean := False;

      procedure Process_Compilation_Unit
        (Comp_Unit : Compilation_Unit);
      --  Process the given compilation unit.
      --  If we have no specified qualifier, look for all the reachable
      --  declarations that match Name in the given unit.
      --  Otherwise, just create an import clause for this unit if
      --  the user speficied this unit in the qualifier.

      function Get_Fully_Qualified_Name
        (Comp_Unit : Compilation_Unit)
         return Langkit_Support.Text.Unbounded_Text_Type;
      --  Return the fully qualified name of the given unit
      --  (e.g: "ada.text_io").
      --  The returned qualified name is in lower-case.

      ------------------------------
      -- Get_Fully_Qualified_Name --
      ------------------------------

      function Get_Fully_Qualified_Name
        (Comp_Unit : Compilation_Unit) return
        Langkit_Support.Text.Unbounded_Text_Type
      is
         Names : constant Unbounded_Text_Type_Array :=
           Comp_Unit.P_Syntactic_Fully_Qualified_Name;
         Fully_Qualified_Name : Langkit_Support.Text.Unbounded_Text_Type;
      begin
         for Name of Names loop
            Fully_Qualified_Name :=
              (if Ada.Strings.Wide_Wide_Unbounded."/="
                 (Fully_Qualified_Name, Null_Unbounded_Wide_Wide_String)
               then
                  Fully_Qualified_Name & "." & Name
               else Name);
         end loop;

         return Fully_Qualified_Name;
      end Get_Fully_Qualified_Name;

      ------------------------------
      -- Process_Compilation_Unit --
      ------------------------------

      procedure Process_Compilation_Unit
        (Comp_Unit : Compilation_Unit) is
      begin
         Stop_Iteration := False;

         if Comp_Unit.Is_Null then
            return;
         end if;

         if Original_Qualifier = Null_Unbounded_Wide_Wide_String then
            Append_Reachable_Declarations
              (Name                   => Name,
               Unit                   => Comp_Unit,
               Reachable_Definitions  => Reachable_Declarations,
               Reachable_Renames      => Reachable_Renames);

         elsif Lowered_Original_Qualifier
           = Get_Fully_Qualified_Name (Comp_Unit)
         then
            --  The qualified specified by the user matches the unit name: stop
            --  processing other units and create an import directive for this
            --  unit.
            Available_Imports.Include
              ((Import    => Original_Qualifier,
                Qualifier => Null_Unbounded_Wide_Wide_String));
            Stop_Iteration := True;
         end if;
      end Process_Compilation_Unit;

   begin
      for Unit of Units loop
         if not Unit.Root.Is_Null
           and then not Unit.Has_Diagnostics
         then
            case Unit.Root.Kind is
               when Ada_Compilation_Unit_Range =>
                  Process_Compilation_Unit  (Unit.Root.As_Compilation_Unit);

                  if Stop_Iteration then
                     return Available_Imports;
                  end if;

               when Ada_Compilation_Unit_List_Range =>
                  for Comp_Unit of Unit.Root.As_Compilation_Unit_List loop
                     Process_Compilation_Unit (Comp_Unit.As_Compilation_Unit);

                     if Stop_Iteration then
                        return Available_Imports;
                     end if;
                  end loop;
               when others =>
                  null;
            end case;
         end if;
      end loop;

      Available_Imports := Create_Available_Imports
        (Name_To_Import         => Name,
         Reachable_Declarations => Reachable_Declarations,
         Reachable_Renames      => Reachable_Renames);

      return Available_Imports;
   end Get_Available_Imports;

   ----------------------------------
   -- Get_Generic_Package_Internal --
   ----------------------------------

   function Get_Generic_Package_Internal
     (Instantiation : Generic_Package_Instantiation)
      return Generic_Package_Internal is
   begin
      return Instantiation.P_Designated_Generic_Decl.
        As_Generic_Package_Decl.F_Package_Decl.As_Generic_Package_Internal;

   exception
      when Langkit_Support.Errors.Property_Error =>
         return No_Generic_Package_Internal;
   end Get_Generic_Package_Internal;

   ----------------------------------------
   -- Get_Insert_New_Dependency_Location --
   ----------------------------------------

   function Get_Insert_New_Dependency_Location
     (Unit           : Compilation_Unit'Class;
      New_Dependency : Langkit_Support.Text.Text_Type)
      return Source_Location
   is
      use Langkit_Support.Text;

      New_Dependency_Lower : constant Text_Type := To_Lower (New_Dependency);

   begin
      if Unit.F_Prelude.Is_Null
        or else Unit.F_Prelude.Children_Count = 0
      then
         --  If there is no prelude, simply insert right before Unit's body.
         --  This ensures that the with clause is inserted after any comments
         --  like copyright headers.
         return Start_Sloc (Unit.F_Body.Sloc_Range);

      else
         for Prelude_Node of Unit.F_Prelude loop
            if Prelude_Node.Kind in Ada_With_Clause_Range then
               --  Handle list of packages: "with A, B, C;"
               for Withed_Package of
                 Prelude_Node.As_With_Clause.F_Packages
               loop
                  if New_Dependency_Lower < To_Lower (Withed_Package.Text) then
                     --  Assuming the prelude is sorted alphabetically,
                     --  the insert location is before the first clause greater
                     --  than New_Dependency.
                     --  With clause lists are not separated. Inseated, the
                     --  insertion is done before the list.
                     return Start_Sloc (Prelude_Node.Sloc_Range);
                  end if;
               end loop;
            end if;
         end loop;

         --  Insert as the last element of the prelude
         return Source_Location'(Unit.F_Prelude.Sloc_Range.End_Line + 1, 1);
      end if;
   end Get_Insert_New_Dependency_Location;

   ------------------------
   -- Get_Parent_Package --
   ------------------------

   function Get_Parent_Packages
     (Defining_Name : Libadalang.Analysis.Defining_Name'Class)
      return Basic_Decl_Vector
   is
      Parent_Packages : Basic_Decl_Vector;

   begin
      for Parent of Defining_Name.Parents (With_Self => False) loop
         if Parent.Kind in
           Ada_Package_Body
           | Ada_Package_Decl
           | Ada_Generic_Package_Decl
         then
            Parent_Packages.Append (Parent.As_Basic_Decl);
         end if;
      end loop;

      declare
         Top_Level_Declaration : constant Basic_Decl :=
           Defining_Name.P_Enclosing_Compilation_Unit.P_Decl;

      begin
         if not Top_Level_Declaration.Is_Null
           and then Top_Level_Declaration.Kind in Ada_Subp_Body_Range
         then
            --  Inside a main file: add the main name as the last element
            --  it will be used to filter out all the elements defined inside
            --  the unit itself.
            Parent_Packages.Append (Top_Level_Declaration);
         end if;
      end;

      if not Parent_Packages.Is_Empty
        and then
          Parent_Packages
            .Last_Element
            .P_Defining_Name
            .P_Parent_Basic_Decl
            .Kind in Ada_Generic_Package_Instantiation
      then
         declare
            Instantiation : constant Basic_Decl :=
              Parent_Packages.Last_Element.P_Defining_Name.P_Parent_Basic_Decl;

         begin
            Parent_Packages.Delete_Last;
            Parent_Packages.Append (Instantiation);
            Parent_Packages.Append
              (Get_Parent_Packages (Instantiation.P_Defining_Name));
         end;
      end if;

      return Parent_Packages;
   end Get_Parent_Packages;

   -----------
   -- Image --
   -----------

   function Image (Object : Import_Type) return String is
      use Langkit_Support.Text;

   begin
      return
        "Import: "
        & To_UTF8 (To_Text (Object.Import))
        & "; Qualifier: " & To_UTF8 (To_Text (Object.Qualifier));
   end Image;

   ------------------------------
   -- Is_Auto_Import_Available --
   ------------------------------

   function Is_Auto_Import_Available
     (Unit              : Analysis_Unit;
      Location          : Source_Location;
      Units             : not null access function return Analysis_Unit_Array;
      Name              : out Libadalang.Analysis.Name;
      Available_Imports : out Import_Type_Ordered_Set)
      return Boolean
   is
      Node : constant Ada_Node := Unit.Root.Lookup (Location);

   begin
      Available_Imports := [];

      if Node.Kind not in Ada_Base_Id then
         return False;
      end if;

      declare
         Is_Imprecise         : Boolean;
         Enclosing_Name : constant Libadalang.Analysis.Name :=
           Get_Appropriate_Enclosing_Name (Node.As_Base_Id);
         Resolved_Name  : constant Defining_Name :=
           Laltools.Common.Resolve_Name
             (Enclosing_Name, LAL_Refactor.Refactor_Trace, Is_Imprecise);

      begin
         if Resolved_Name.Is_Null then
            Me.Trace
              ("Can't resolve name "
               & Enclosing_Name.Image
               & ": check for available imports");

            Name := Enclosing_Name;
            Available_Imports :=
              Get_Available_Imports
                (Name  => Enclosing_Name,
                 Units => Units.all);

            return not Available_Imports.Is_Empty;

         else
            Me.Trace
              (Enclosing_Name.Image
               & " can be resolved (imprecise: "
               & Is_Imprecise'Img
               & "): "
               & "no need to check for missing imports");
            return False;
         end if;
      end;
   end Is_Auto_Import_Available;

   ------------------------------------------
   -- List_And_Expand_Package_Declarations --
   ------------------------------------------

   function List_And_Expand_Package_Declarations
     (Package_Decl    : Base_Package_Decl'Class;
      Include_Private : Boolean)
      return Basic_Decl_Vector
   is
      Result : Basic_Decl_Vector;

      procedure Explore_Declarative_Part (Decls : Ada_Node_List);
      --  Iterates through Decls adding each declaration to All_Decls. If
      --  a declaration is a Package_Decl then it is expanded and nested
      --  declarations are also added to All_Decls

      ------------------------------
      -- Explore_Declarative_Part --
      ------------------------------

      procedure Explore_Declarative_Part (Decls : Ada_Node_List) is
      begin
         for Node of Decls loop
            if Node.Kind in Ada_Basic_Decl then
               --  F_Decls can return nodes that not not inherit from
               --  Basic_Decl type, so ignore those.

               if Node.Kind = Ada_Package_Decl then
                  --  Expand again nested Package_Decl nodes.

                  for Nested_Node of List_And_Expand_Package_Declarations
                    (Node.As_Package_Decl, False)
                  loop
                     Result.Append (Nested_Node.As_Basic_Decl);
                  end loop;
               elsif Node.Kind = Ada_Generic_Package_Instantiation
               then
                  --  Expand again nested Generic_Package_Instantiation nodes.

                  for Nested_Node of List_And_Expand_Package_Declarations
                    (Get_Generic_Package_Internal
                       (Node.As_Generic_Package_Instantiation),
                     False)
                  loop
                     Result.Append (Nested_Node.As_Basic_Decl);
                  end loop;
               else
                  --  All other nodes that inherit from Basic_Decl type except
                  --  Package_Decl and Generic_Package_Instantiation can
                  --  be added.

                  Result.Append (Node.As_Basic_Decl);
               end if;
            end if;
         end loop;
      end Explore_Declarative_Part;

   begin
      --  Return an empty set if Package_Decl is null
      if Package_Decl.Is_Null then
         return Result;
      end if;

      if not Package_Decl.F_Public_Part.Is_Null then
         begin
            Explore_Declarative_Part (Package_Decl.F_Public_Part.F_Decls);

         exception
            --  Ignore Constraint_Error thrown by F_Decls due to invalid code.
            --  Continue execution since Pkg_Decl.F_Private_Part.F_Decls might
            --  not have the same issue.
            --  This has been seen in V516-059.
            when Constraint_Error =>
               null;
         end;
      end if;

      if Include_Private and then not Package_Decl.F_Private_Part.Is_Null then
         begin
            Explore_Declarative_Part (Package_Decl.F_Private_Part.F_Decls);

         exception
            --  Ignore Constraint_Error thrown by F_Decls due to invalid code.
            --  Continue execution since Pkg_Decl.F_Public_Part.F_Decls might
            --  have executed correctly.
            --  This has been seen in V516-059.
            when Constraint_Error =>
               null;
         end;
      end if;

      return Result;
   end List_And_Expand_Package_Declarations;

   --------------
   -- Refactor --
   --------------

   overriding
   function Refactor
     (Self           : Auto_Importer;
      Analysis_Units : access function return Analysis_Unit_Array)
      return Refactoring_Edits
   is
      use Langkit_Support.Text;

      Result : Refactoring_Edits;

      procedure Add_Import;
      --  Creates a Text_Edit with the necessary import and adds it to result

      procedure Add_Qualifier;
      --  Creates a Text_Edit with the necessary qualifier and adds it to
      --  result.

      ----------------
      -- Add_Import --
      ----------------

      procedure Add_Import is
         function Has_With_Clause (Unit : Compilation_Unit) return Boolean
           with Pre => not Unit.Is_Null;
         --  Checks if Unit has a With_Clause with the
         --  Self.Import.Import package.

         function Has_Use_Clause (Unit : Compilation_Unit) return Boolean
           with Pre => not Unit.Is_Null;
         --  Checks if Unit has a Use_Package_Clause with the
         --  Self.Import.Import package.

         procedure Remove_With_Clause (Unit : Compilation_Unit)
           with Pre => not Unit.Is_Null;
         --  Creates a Text_Edit that removes the With_Clause with the
         --  Self.Import.Import package from Unit.

         procedure Add_With_Clause (Unit : Compilation_Unit)
           with Pre => not Unit.Is_Null;
         --  Creates a Text_Edit that adds a With_Clause with the
         --  Self.Import.Import package from Unit.

         ---------------------
         -- Has_With_Clause --
         ---------------------

         function Has_With_Clause (Unit : Compilation_Unit) return Boolean
         is
            Import : constant Text_Type :=
              To_Lower (To_Text (Self.Import.Import));

         begin
            for Node of Unit.F_Prelude loop
               if Node.Kind in Ada_With_Clause_Range then
                  for With_Clause of Node.As_With_Clause.F_Packages loop
                     if Import = To_Lower (With_Clause.Text) then
                        return True;
                     end if;
                  end loop;
               end if;
            end loop;

            return False;
         end Has_With_Clause;

         --------------------
         -- Has_Use_Clause --
         --------------------

         function Has_Use_Clause (Unit : Compilation_Unit) return Boolean
         is
            Import : constant Text_Type :=
              To_Lower (To_Text (Self.Import.Import));

         begin
            for Node of Unit.F_Prelude loop
               if Node.Kind in Ada_Use_Package_Clause_Range then
                  for Use_Clause of Node.As_Use_Package_Clause.F_Packages loop
                     if Import = To_Lower (Use_Clause.Text) then
                        return True;
                     end if;
                  end loop;
               end if;
            end loop;

            return False;
         end Has_Use_Clause;

         ------------------------
         -- Remove_With_Clause --
         ------------------------

         procedure Remove_With_Clause (Unit : Compilation_Unit)
         is
            Import : constant Text_Type :=
              To_Lower (To_Text (Self.Import.Import));

            procedure Remove_With_Clause (Clause : Name'Class)
              with Pre =>
                not Clause.Is_Null
                and then Clause.Parent.Kind in Ada_Name_List_Range
                and then Clause.Parent.Parent.Kind in Ada_With_Clause_Range;
            --  Creates a Text_Edit that removes Clause from it's parent
            --  which is a list of the withed packages.

            ------------------------
            -- Remove_With_Clause --
            ------------------------

            procedure Remove_With_Clause (Clause : Name'Class) is
               Individual : constant Boolean :=
                 Clause.Previous_Sibling.Is_Null
                 and then Clause.Next_Sibling.Is_Null;
               First      : constant Boolean :=
                 not Individual
                 and then Clause.Previous_Sibling.Is_Null;

            begin
               if Individual then
                  declare
                     With_Clause              :
                       constant Libadalang.Analysis.With_Clause :=
                         Clause.Parent.Parent.As_With_Clause;
                     With_Clause_Sloc_Range   :
                       constant Source_Location_Range :=
                         With_Clause.Sloc_Range;
                     With_Clause_Next_Sibling : constant Ada_Node :=
                       With_Clause.Next_Sibling;
                     Remove_Whole_Line : constant Boolean :=
                       With_Clause_Next_Sibling.Is_Null
                       or else With_Clause_Next_Sibling.Sloc_Range.Start_Line
                                 /= With_Clause_Sloc_Range.End_Line;

                     Edit : constant Text_Edit :=
                       (Location =>
                          (if Remove_Whole_Line then
                             Make_Range
                               (Start_Sloc (With_Clause_Sloc_Range),
                                (Line   => With_Clause_Sloc_Range.End_Line + 1,
                                 Column => 1))

                           else
                             Make_Range
                               (Start_Sloc (With_Clause_Sloc_Range),
                                Start_Sloc
                                  (With_Clause_Next_Sibling.Sloc_Range))),
                        Text     => Null_Unbounded_String);

                  begin
                     Safe_Insert
                       (Result.Text_Edits, Unit.Unit.Get_Filename, Edit);
                  end;

               elsif First then
                  declare
                     Edit : constant Text_Edit :=
                       (Location =>
                          Make_Range
                            (Start_Sloc (Clause.Sloc_Range),
                             Start_Sloc (Clause.Next_Sibling.Sloc_Range)),
                        Text     => Null_Unbounded_String);

                  begin
                     Safe_Insert
                       (Result.Text_Edits, Unit.Unit.Get_Filename, Edit);
                  end;

               else
                  declare
                     Edit : constant Text_Edit :=
                       (Location =>
                          Make_Range
                            (Start_Sloc (Clause.Previous_Sibling.Sloc_Range),
                             Start_Sloc (Clause.Sloc_Range)),
                        Text     => Null_Unbounded_String);

                  begin
                     Safe_Insert
                       (Result.Text_Edits, Unit.Unit.Get_Filename, Edit);
                  end;
               end if;

            end Remove_With_Clause;

         begin
            for Node of Unit.F_Prelude loop
               if Node.Kind in Ada_With_Clause_Range then
                  for With_Clause of Node.As_With_Clause.F_Packages loop
                     if Import = To_Lower (With_Clause.Text) then
                        Remove_With_Clause (With_Clause);

                        return;
                     end if;
                  end loop;
               end if;
            end loop;
         end Remove_With_Clause;

         ---------------------
         -- Add_With_Clause --
         ---------------------

         procedure Add_With_Clause (Unit : Compilation_Unit)
         is
            Insert_Location    : constant Source_Location :=
              Get_Insert_New_Dependency_Location
                (Unit, To_Text (Self.Import.Import));

            Edit : constant Text_Edit :=
              (Location =>
                 Make_Range (Insert_Location, Insert_Location),
               Text     =>
                 ("with "
                  & To_Unbounded_String
                      (To_UTF8 (To_Text (Self.Import.Import)))
                  & ";"
                  & Ada.Characters.Latin_1.LF));

         begin
            Safe_Insert (Result.Text_Edits, Unit.Unit.Get_Filename, Edit);
         end Add_With_Clause;

         Unit : constant Compilation_Unit :=
           Self.Name.P_Enclosing_Compilation_Unit;
         Kind : constant Analysis_Unit_Kind := Unit.P_Unit_Kind;

      begin
         case Kind is
            when Unit_Specification =>
               declare
                  Other_Unit : constant Compilation_Unit := Unit.P_Other_Part;

               begin
                  if Has_With_Clause (Unit) then
                     null;
                  else
                     Add_With_Clause (Unit);
                     if not Other_Unit.Is_Null
                       and then Has_With_Clause (Other_Unit)
                       and then not Has_Use_Clause (Other_Unit)
                     then
                        Remove_With_Clause (Other_Unit);
                     end if;
                  end if;
               end;

            when Unit_Body =>
               declare
                  Other_Unit : constant Compilation_Unit := Unit.P_Other_Part;

               begin
                  if Has_With_Clause (Unit)
                    or (not Other_Unit.Is_Null
                        and then Has_With_Clause (Other_Unit))
                  then
                     null;

                  else
                     Add_With_Clause (Unit);
                  end if;
               end;

         end case;
      end Add_Import;

      -------------------
      -- Add_Qualifier --
      -------------------

      procedure Add_Qualifier is
         use Ada.Strings.Wide_Wide_Unbounded;

         Qualifier : constant Text_Edit :=
           (if Self.Name.Parent.Kind in Ada_Dotted_Name then
              Text_Edit'
                (Location =>
                   Self.Name.Parent.As_Dotted_Name.F_Prefix.Sloc_Range,
                 Text     =>
                   To_Unbounded_String
                     (To_UTF8 (To_Text (Self.Import.Qualifier))))
            else
              Text_Edit'
                (Location =>
                   Make_Range
                     (Start_Sloc (Self.Name.Sloc_Range),
                      Start_Sloc (Self.Name.Sloc_Range)),
                 Text     =>
                   (if Ada.Strings.Wide_Wide_Unbounded."="
                         (Self.Import.Qualifier,
                          Null_Unbounded_Wide_Wide_String)
                    then
                       To_Unbounded_String
                         (To_UTF8 (To_Text (Self.Import.Qualifier)))
                    else
                       To_Unbounded_String
                         (To_UTF8 (To_Text (Self.Import.Qualifier)))
                       & ".")));

      begin
         Safe_Insert
           (Result.Text_Edits, Self.Name.Unit.Get_Filename, Qualifier);
      end Add_Qualifier;

   begin
      Add_Import;
      if not Ada.Strings.Wide_Wide_Unbounded."="
        (Self.Import.Qualifier,
         Ada.Strings.Wide_Wide_Unbounded.Null_Unbounded_Wide_Wide_String)
      then
         Add_Qualifier;
      end if;

      return Result;
   end Refactor;

end LAL_Refactor.Auto_Import;
