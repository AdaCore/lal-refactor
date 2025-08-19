--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Laltools.Common;
with Langkit_Support.Text;
with Libadalang.Common;

package body LAL_Refactor.Delete_Entity is

   use all type Libadalang.Common.Ada_Node_Kind_Type;

   function Find_Parent
     (Node      : Ada_Node'Class;
      Predicate : not null access
        function (Node : Ada_Node'Class) return Boolean)
      return Ada_Node;
   --  Find nearest parent of Node passing given Predicate

   function Is_Safe_To_Delete
     (Declaration : Basic_Decl;
      Referencce  : Base_Id'Class) return Boolean;
   --  Check if it is safe to delete given Reference of the Declaration

   function Is_Single_Item_List (List : Ada_Node_List) return Boolean is
     (not List.Ada_Node_List_Has_Element (2));

   function Is_Block_Statement (Stmt_List : Ada_Node_List) return Boolean is
     (Stmt_List.Parent.Parent.Kind in Libadalang.Common.Ada_Block_Stmt);

   package Diagnostics is

      type Diagnostic is new Refactoring_Diagnostic with record
         Reference : Base_Id;
      end record;

      function Create (Value : Base_Id) return Diagnostic is
        (Reference => Value);

      overriding
      function Filename (Self : Diagnostic) return String is
        (Self.Reference.Unit.Get_Filename);

      overriding
      function Location (Self : Diagnostic) return Source_Location_Range is
        (Self.Reference.Sloc_Range);

      function Info (Self : Diagnostic) return String is
        ("Usage of " &
           Langkit_Support.Text.Image (Self.Reference.Text, True) &
           " that is not safe to delete.");

   end Diagnostics;

   --------------
   -- Refactor --
   --------------

   overriding
   function Refactor
     (Self           : Entity_Remover;
      Analysis_Units : access function return Analysis_Unit_Array)
      return Refactoring_Edits
   is
      use all type Libadalang.Common.Ref_Result_Kind;

      function Is_Inside_Parts
        (Ref   : Base_Id'Class;
         Parts : Basic_Decl_Array) return Boolean is
        (for some Parent of Ref.Parents =>
           (for some Part of Parts =>
                 Part = Parent));
      --  Check if Ref is located inside one of Parts.

      function Is_Statement (Node : Ada_Node'Class) return Boolean is
        (not Node.Is_Null and then Node.Kind in Libadalang.Common.Ada_Stmt);

      Declaration : constant Basic_Decl :=
        Self.Definition.P_Basic_Decl;

      Parts       : constant Basic_Decl_Array :=
        Declaration.P_All_Parts;

      Refs        : constant Ref_Result_Array :=
        Self.Definition.P_Find_All_References
          (Units              => Analysis_Units.all,
           Follow_Renamings   => False,
           Imprecise_Fallback => False);
   begin
      return Result : Refactoring_Edits do
         for Item of Refs when Kind (Item) = Precise loop
            declare
               Ref  : constant Base_Id'Class := Libadalang.Analysis.Ref (Item);
               Stmt : Ada_Node;
               List : Ada_Node_List;
            begin
               if Is_Inside_Parts (Ref, Parts) then
                  null;  --  Do no analisys inside declaration parts
               elsif Is_Safe_To_Delete (Declaration, Ref) then
                  Stmt := Find_Parent (Ref, Is_Statement'Access);
                  List := Stmt.Parent.As_Ada_Node_List;

                  if not Is_Single_Item_List (List) then
                     Remove_Node (Result.Text_Edits, Stmt, Expand => True);
                  elsif Is_Block_Statement (List) then
                     Remove_Node
                       (Result.Text_Edits,
                        List.Parent.Parent,
                        Expand => True);
                  else
                     Replace_Node
                       (Result.Text_Edits,
                        Stmt,
                        To_Unbounded_String ("null;"));
                  end if;

               else
                  Result.Diagnostics.Append
                    (Diagnostics.Create (Ref.As_Base_Id));
               end if;
            end;
         end loop;

         for Part of Parts loop
            Remove_Node (Result.Text_Edits, Part, Expand => True);
         end loop;
      end return;
   end Refactor;

   -----------------
   -- Find_Parent --
   -----------------

   function Find_Parent
     (Node      : Ada_Node'Class;
      Predicate : not null access
        function (Node : Ada_Node'Class) return Boolean)
      return Ada_Node
   is
      Next : Ada_Node := Node.Parent;
   begin
      while not Next.Is_Null
        and then not Predicate (Next)
      loop
         Next := Next.Parent;
      end loop;

      return Next;
   end Find_Parent;

   --------------------------------
   -- Is_Delete_Entity_Available --
   --------------------------------

   function Is_Delete_Entity_Available
     (Unit      : Analysis_Unit;
      Node_SLOC : Source_Location)
      return Boolean
   is
      Node : constant Ada_Node :=
        (if Unit = No_Analysis_Unit then No_Ada_Node else
            Unit.Root.Lookup (Node_SLOC));

      Definition : constant Defining_Name :=
        (if Node.Is_Null or else Node.Kind not in Libadalang.Common.Ada_Name
         then No_Defining_Name
         else Laltools.Common.Resolve_Name_Precisely (Node.As_Name));

      Declaration : constant Basic_Decl :=
        (if Definition.Is_Null
         then No_Basic_Decl
         else Definition.P_Basic_Decl);
   begin
      return not Declaration.Is_Null
        and then
          (case Declaration.Kind is
              when Ada_Subp_Decl =>
                Declaration.As_Classic_Subp_Decl.F_Subp_Spec.F_Subp_Kind
                  in Ada_Subp_Kind_Procedure,
              when Ada_Subp_Body | Ada_Subp_Renaming_Decl =>
                Declaration.As_Base_Subp_Body.F_Subp_Spec.F_Subp_Kind
                  in Ada_Subp_Kind_Procedure,
              when Ada_Entry_Decl => True,
              when Ada_Null_Subp_Decl => True,
              when others => False);
   end Is_Delete_Entity_Available;

   -----------------------
   -- Is_Safe_To_Delete --
   -----------------------

   function Is_Safe_To_Delete
     (Declaration : Basic_Decl;
      Referencce  : Base_Id'Class) return Boolean is
   begin
      case Declaration.Kind is
         when Ada_Entry_Decl
            | Ada_Subp_Decl
            | Ada_Subp_Body
            | Ada_Subp_Renaming_Decl
            | Ada_Null_Subp_Decl =>
            --  It's safe to delete procedure/entry calls
            return Referencce.P_Is_Call;
         when others =>
            return False;
      end case;
   end Is_Safe_To_Delete;

   ---------------------------
   -- Create_Entity_Deleter --
   ---------------------------

   function Create_Entity_Deleter
     (Unit            : Analysis_Unit;
      Definition_SLOC : Source_Location)
      return Entity_Remover is
     (declare
        Node : constant Ada_Node :=
          (if Unit = No_Analysis_Unit then No_Ada_Node else
              Unit.Root.Lookup (Definition_SLOC));

        Definition : constant Defining_Name :=
          (if Node.Is_Null or else Node.Kind not in Libadalang.Common.Ada_Name
           then No_Defining_Name
           else Laltools.Common.Resolve_Name_Precisely (Node.As_Name));
      begin
        (if Definition.Is_Null
         then
            raise Invalid_Declaration with
              "failed to resolve "
              & Unit.Get_Filename
              & ":"
              & Image (Definition_SLOC)
         else
           (Refactoring_Tool with Definition => Definition)));

end LAL_Refactor.Delete_Entity;
