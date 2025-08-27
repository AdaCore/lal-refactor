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
      Reference  : Name) return Boolean;
   --  Check if it is safe to delete given Reference of the Declaration

   function Is_Single_Item_List (List : Ada_Node_List'Class) return Boolean is
     (not List.Ada_Node_List_Has_Element (2));

   function Is_Single_Item_List (List : Defining_Name_List) return Boolean is
     (not List.Defining_Name_List_Has_Element (2));

   function Is_Single_Item_List
     (List : Case_Stmt_Alternative_List) return Boolean is
       (not List.Case_Stmt_Alternative_List_Has_Element (2));

   function Is_Single_Item_List
     (List : Case_Expr_Alternative_List) return Boolean is
       (not List.Case_Expr_Alternative_List_Has_Element (2));

   function Is_Single_Item_List
     (List : Basic_Assoc_List'Class) return Boolean is
       (not List.Basic_Assoc_List_Has_Element (2));

   function Is_Single_Item_List (List : Variant_List) return Boolean is
     (not List.Variant_List_Has_Element (2));

   function Is_Block_Statement (Stmt_List : Ada_Node_List) return Boolean is
     (Stmt_List.Parent.Parent.Kind in Libadalang.Common.Ada_Block_Stmt);

   function Is_Single_Result_Procedure_Call (Arg : Name) return Boolean;
   --  Check if Arg is an argument to a procedure call with a single out/in-out
   --  parameter.

   function To_Selected_Name (Ref : Base_Id'Class) return Name;
   --  Return selected_name for given identifier, like A.B.C for C

   function All_Parts (Node : Defining_Name) return Basic_Decl_Array is
     [for Name of Node.P_All_Parts => Name.P_Basic_Decl];

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

      procedure Remove_Alternative
        (Result : in out Refactoring_Edits;
         Ref    : Name)
           with Pre => Ref.Parent.Kind = Ada_Alternatives_List;

      procedure Remove_Pragma
        (Result : in out Refactoring_Edits;
         Ref    : Name);

      procedure Remove_Exception_Handler
        (Result : in out Refactoring_Edits;
         Ref    : Name);

      procedure Remove_Case_Stmt_Alternative
        (Result : in out Refactoring_Edits;
         Ref    : Name);

      procedure Remove_Case_Expr_Alternative
        (Result : in out Refactoring_Edits;
         Ref    : Name);

      procedure Remove_Aggregate_Assoc
        (Result : in out Refactoring_Edits;
         Ref    : Name);

      procedure Remove_Variant
        (Result : in out Refactoring_Edits;
         Ref    : Name);

      procedure Remove_Statement
        (Result : in out Refactoring_Edits;
         Ref    : Name);

      function Is_Inside_Parts
        (Ref   : Base_Id'Class;
         Parts : Basic_Decl_Array) return Boolean is
        (for some Parent of Ref.Parents =>
           (for some Part of Parts =>
                 Part = Parent));
      --  Check if Ref is located inside one of Parts.

      function Is_Statement (Node : Ada_Node'Class) return Boolean is
        (not Node.Is_Null and then Node.Kind in Libadalang.Common.Ada_Stmt);

      ----------------------------
      -- Remove_Aggregate_Assoc --
      ----------------------------

      procedure Remove_Aggregate_Assoc
        (Result : in out Refactoring_Edits;
         Ref    : Name)
      is
         Assoc : constant Ada_Node := Ref.Parent.Parent;
      begin
         --  TBD: Check array type is unconstrained?
         pragma Assert
           (not Is_Single_Item_List (Assoc.Parent.As_Assoc_List));

         Remove_Node_And_Delimiter (Result.Text_Edits, Assoc);
      end Remove_Aggregate_Assoc;

      ------------------------
      -- Remove_Alternative --
      ------------------------

      procedure Remove_Alternative
        (Result : in out Refactoring_Edits;
         Ref    : Name) is
      begin
         if not Is_Single_Item_List
           (Ref.Parent.As_Alternatives_List)
         then
            Remove_Node_And_Delimiter (Result.Text_Edits, Ref);
         elsif Ref.Parent.Parent.Kind = Ada_Exception_Handler then
            Remove_Exception_Handler (Result, Ref);
         elsif Ref.Parent.Parent.Kind = Ada_Case_Stmt_Alternative then
            Remove_Case_Stmt_Alternative (Result, Ref);
         elsif Ref.Parent.Parent.Kind = Ada_Case_Expr_Alternative then
            Remove_Case_Expr_Alternative (Result, Ref);
         elsif Ref.Parent.Parent.Kind = Ada_Aggregate_Assoc then
            Remove_Aggregate_Assoc (Result, Ref);
         elsif Ref.Parent.Parent.Kind = Ada_Variant then
            Remove_Variant (Result, Ref);
         else
            raise Program_Error;
         end if;
      end Remove_Alternative;

      ----------------------------------
      -- Remove_Case_Expr_Alternative --
      ----------------------------------

      procedure Remove_Case_Expr_Alternative
        (Result : in out Refactoring_Edits;
         Ref    : Name)
      is
         Alternative : constant Ada_Node := Ref.Parent.Parent;
      begin
         pragma Assert
           (not Is_Single_Item_List
              (Alternative.Parent.As_Case_Expr_Alternative_List));

         Remove_Node_And_Delimiter (Result.Text_Edits, Alternative);
      end Remove_Case_Expr_Alternative;

      ----------------------------------
      -- Remove_Case_Stmt_Alternative --
      ----------------------------------

      procedure Remove_Case_Stmt_Alternative
        (Result : in out Refactoring_Edits;
         Ref    : Name)
      is
         Alternative : constant Ada_Node := Ref.Parent.Parent;
      begin
         if Is_Single_Item_List
           (Alternative.Parent.As_Case_Stmt_Alternative_List)
         then
            Remove_Statement (Result, Ref);
         else
            Remove_Node_And_Delimiter (Result.Text_Edits, Alternative);
         end if;
      end Remove_Case_Stmt_Alternative;

      ------------------------------
      -- Remove_Exception_Handler --
      ------------------------------

      procedure Remove_Exception_Handler
        (Result : in out Refactoring_Edits;
         Ref    : Name)
      is
         Handler : constant Ada_Node := Ref.Parent.Parent;
      begin
         if Is_Single_Item_List (Handler.Parent.As_Ada_Node_List) then
            declare
               Token : constant Libadalang.Common.Token_Reference :=
                 Libadalang.Common.Previous
                   (Handler.Parent.Token_Start,
                    Exclude_Trivia => True);
               --  `exception` token

               Data  : constant Libadalang.Common.Token_Data_Type :=
                 Libadalang.Common.Data (Token);

               SLOC  : Source_Location_Range;
            begin
               SLOC := Make_Range
                 (Start_Sloc (Libadalang.Common.Sloc_Range (Data)),
                  End_Sloc (Handler.Parent.Sloc_Range));

               SLOC := Laltools.Common.Expand_SLOC_Range (Ref.Unit, SLOC);

               Safe_Insert
                 (Result.Text_Edits,
                  Ref.Unit.Get_Filename,
                  (Location => SLOC,
                   Text     => Null_Unbounded_String));
            end;

         else
            Remove_Node_And_Delimiter (Result.Text_Edits, Handler);

         end if;
      end Remove_Exception_Handler;

      ----------------------
      -- Remove_Statement --
      ----------------------

      procedure Remove_Statement
        (Result : in out Refactoring_Edits;
         Ref    : Name)
      is
         Stmt : constant Ada_Node := Find_Parent (Ref, Is_Statement'Access);
         List : constant Ada_Node_List := Stmt.Parent.As_Ada_Node_List;
      begin
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
      end Remove_Statement;

      --------------------
      -- Remove_Variant --
      --------------------

      procedure Remove_Variant
        (Result : in out Refactoring_Edits;
         Ref    : Name)
      is
         Variant : constant Ada_Node := Ref.Parent.Parent;
      begin
         if not Is_Single_Item_List (Variant.Parent.As_Variant_List) then
            Remove_Node_And_Delimiter (Result.Text_Edits, Variant);
         elsif Is_Single_Item_List
           (Variant.Parent.Parent.Parent.As_Component_List.F_Components)
         then
            Replace_Node
              (Result.Text_Edits,
               Variant.Parent.Parent,
               To_Unbounded_String ("null;"));
         else
            Remove_Node
              (Result.Text_Edits,
               Variant.Parent.Parent,
               Expand => True);
         end if;
      end Remove_Variant;

      -------------------
      -- Remove_Pragma --
      -------------------

      procedure Remove_Pragma
        (Result : in out Refactoring_Edits;
         Ref    : Name)
      is
         Pragma_Node : constant Ada_Node :=
           Ref.Parent.Parent.Parent;
      begin
         pragma Assert (Pragma_Node.Kind = Ada_Pragma_Node);

         Remove_Node
           (Result.Text_Edits, Pragma_Node, Expand => True);
      end Remove_Pragma;

      Parts : constant Basic_Decl_Array := All_Parts (Self.Definition);

      Refs  : constant Ref_Result_Array :=
        Self.Definition.P_Find_All_References
          (Units              => Analysis_Units.all,
           Follow_Renamings   => False,
           Imprecise_Fallback => False);

      Declaration : constant Basic_Decl :=
        Self.Definition.P_Basic_Decl;

   begin
      return Result : Refactoring_Edits do
         --  Delete all references
         for Item of Refs when Kind (Item) = Precise loop
            declare
               Ref : constant Base_Id'Class := Libadalang.Analysis.Ref (Item);

               Name : constant Libadalang.Analysis.Name :=
                 To_Selected_Name (Ref);
            begin
               if Is_Inside_Parts (Ref, Parts) then
                  null;  --  Do no analisys inside declaration parts

               elsif not Is_Safe_To_Delete (Declaration, Name) then
                  Result.Diagnostics.Append
                    (Diagnostics.Create (Ref.As_Base_Id));

               elsif Name.Parent.Kind = Ada_Pragma_Argument_Assoc then
                  Remove_Pragma (Result, Name);

               elsif Name.Parent.Kind = Ada_Alternatives_List then
                  Remove_Alternative (Result, Name);

               else
                  Remove_Statement (Result, Name);
               end if;
            end;
         end loop;

         --  Delete all definitions
         for Name of Self.Definition.P_All_Parts loop
            --  Check if we have a declaration with multiple defining names
            if Name.P_Basic_Decl.Kind in Ada_Exception_Decl | Ada_Object_Decl
              and then not Is_Single_Item_List
                (Name.Parent.As_Defining_Name_List)
            then
               --  Remove defining name from the list
               Remove_Node_And_Delimiter (Result.Text_Edits, Name);
            else
               --  Remove whole declaration
               Remove_Node
                 (Result.Text_Edits, Name.P_Basic_Decl, Expand => True);
            end if;
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
              when Ada_Exception_Decl => True,
              when Ada_Object_Decl => True,
              when others => False);
   end Is_Delete_Entity_Available;

   -----------------------
   -- Is_Safe_To_Delete --
   -----------------------

   function Is_Safe_To_Delete
     (Declaration : Basic_Decl;
      Reference   : Name) return Boolean is
   begin
      if Reference.Parent.Kind = Ada_Pragma_Argument_Assoc then
         --  Any pragma with the reference is safe to delete
         return True;
      end if;

      if Reference.Parent.Kind = Ada_Alternatives_List
        and then Is_Single_Item_List (Reference.Parent.As_Alternatives_List)
      then
         --  We can't delete the last (single-item) alternative in next cases:
         case Reference.Parent.Parent.Kind is
            when Ada_Case_Expr_Alternative =>
               if Is_Single_Item_List
                 (Reference.Parent.Parent.Parent.As_Case_Expr_Alternative_List)
               then
                  return False;
               end if;
            when Ada_Aggregate_Assoc =>
               if Is_Single_Item_List
                 (Reference.Parent.Parent.Parent.As_Assoc_List)
               then
                  return False;
               end if;
            when others =>
               null;
         end case;
      end if;

      case Declaration.Kind is
         when Ada_Entry_Decl
            | Ada_Subp_Decl
            | Ada_Subp_Body
            | Ada_Subp_Renaming_Decl
            | Ada_Null_Subp_Decl =>
            --  It's safe to delete procedure/entry calls
            return Reference.P_Is_Call;
         when Ada_Exception_Decl =>
            --  It's safe to delete corresponding handlers
            return Reference.Parent.Parent.Kind = Ada_Exception_Handler;
         when Ada_Object_Decl =>
            declare
               Parent : constant Ada_Node := Reference.Parent;
            begin
               --  It's safe to delete assignment statements if ref is LHS or
               --  a call to procedure that has a single out/in-out parameter,
               --  like `Float_IO.Get (Input, To_Be_Deleted_Var);`
               --  or part of Alternatives_List
               return
                 (Parent.Kind = Ada_Assign_Stmt
                    and then Parent.As_Assign_Stmt.F_Dest = Reference)
                 or else Is_Single_Result_Procedure_Call (Reference)
                 or else Parent.Kind = Ada_Alternatives_List;
            end;
         when others =>
            return False;
      end case;
   end Is_Safe_To_Delete;

   -------------------------------------
   -- Is_Single_Result_Procedure_Call --
   -------------------------------------

   function Is_Single_Result_Procedure_Call (Arg : Name) return Boolean is
      function Is_Call_Statement (Node : Ada_Node'Class) return Boolean is
        (Node.Kind in Ada_Call_Stmt);

      Node  : constant Ada_Node := Find_Parent (Arg, Is_Call_Statement'Access);
      Count : Natural := 0;
   begin
      if Node.Is_Null
        or else Node.As_Call_Stmt.F_Call.Is_Null
        or else not Node.As_Call_Stmt.F_Call.P_Is_Call
      then
         return False;
      end if;

      for Item of Node.As_Call_Stmt.F_Call.P_Call_Params loop
         declare
            Param : constant Param_Spec :=
              (if Item.Param.P_Basic_Decl.Kind = Ada_Param_Spec
               then Item.Param.P_Basic_Decl.As_Param_Spec
               else No_Param_Spec);
         begin
            if Item.Actual = Arg then
               Count := Count + 1;

               if Param.F_Mode not in Libadalang.Common.Ada_Mode_Out
                   | Libadalang.Common.Ada_Mode_In_Out
                 or else not Is_Single_Item_List (Param.F_Ids)
               then
                  return False;
               end if;
            elsif Param.F_Mode in Libadalang.Common.Ada_Mode_Out
              | Libadalang.Common.Ada_Mode_In_Out
            then
               return False;
            end if;
         end;
      end loop;

      return Count = 1;
   end Is_Single_Result_Procedure_Call;

   ----------------------
   -- To_Selected_Name --
   ----------------------

   function To_Selected_Name (Ref : Base_Id'Class) return Name is
      Result : Name := Ref.As_Name;
   begin
      if not Result.Parent.Is_Null
        and then Result.Parent.Kind = Ada_Dotted_Name
        and then Result.Parent.As_Dotted_Name.F_Suffix = Ref
      then
         Result := Result.Parent.As_Name;
      end if;

      return Result;
   end To_Selected_Name;

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
