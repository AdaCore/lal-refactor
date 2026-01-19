--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Wide_Wide_Unbounded;

with Ada.Wide_Wide_Characters;
with Libadalang.Common;    use Libadalang.Common;
with Laltools.Common;      use Laltools.Common;
with Langkit_Support.Text;

with LAL_Refactor.Tools;   use LAL_Refactor.Tools;

package body LAL_Refactor.Sort_Case is

   Tool_Name : constant String := "Sort Case";

   function Is_Case (Node : Ada_Node'Class) return Boolean is
     (not Node.Is_Null and then Node.Kind in Ada_Case_Stmt);

   function Is_Concrete_Type_Decl (Node : Ada_Node'Class) return Boolean is
     (not Node.Is_Null and then Node.Kind in Ada_Concrete_Type_Decl);

   package Sets is new Ada.Containers.Indefinite_Ordered_Sets
     (Langkit_Support.Text.Text_Type);

   package Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Langkit_Support.Text.Text_Type, Langkit_Support.Text.Text_Type);

   type Literal_Info is record
      Name       : Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String;
      Text       : Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String;
      Text_First : Natural;
      Text_Last  : Natural;
      SLOC       : Source_Location_Range;
   end record;

   package Literal_Vectors is new Ada.Containers.Indefinite_Vectors
     (Positive, Literal_Info);

   package Text_Vectors is new Ada.Containers.Indefinite_Vectors
     (Positive, Langkit_Support.Text.Text_Type);

   procedure Get_Delaration
     (Node   : Ada_Node;
      Vector : out Text_Vectors.Vector);
   --  Fills Vector with the declaration's literals in the order
   --  they are declared

   --------------------------------------
   -- Is_Sort_Alphabetically_Available --
   --------------------------------------

   function Is_Sort_Alphabetically_Available
     (Unit     : Analysis_Unit;
      Location : in out Source_Location)
      return Boolean
   is
      use Sets;

      Token : constant Token_Reference := Unit.Lookup_Token (Location);
      Node  : constant Ada_Node        := Find_Parent
        (Lookup (Unit, Token, Forward),
         Is_Case'Access);

      Names : Sets.Set;
      C     : Sets.Cursor;
      Dummy : Boolean;
   begin
      if Node.Is_Null then
         return False;
      end if;

      --  Node is CaseStmt
      for Aux of Node.Children when not Aux.Is_Null loop
         if Aux.Kind = Ada_Case_Stmt_Alternative_List then
            --  Aux is list of `when`es
            for Case_Stmt_Alternative of Aux.Children
              when not Case_Stmt_Alternative.Is_Null
            loop
               --  Case_Stmt_Alternative is one `when =>`
               for Alternative_List of Case_Stmt_Alternative.Children
                 when not Alternative_List.Is_Null
               loop
                  if Alternative_List.Kind = Ada_Alternatives_List then
                     --  Alternative_List is the list of identifiers in
                     --  the `when .. =>`
                     if Alternative_List.Child
                       (Alternative_List.First_Child_Index).Kind =
                       Ada_Others_Designator
                     then
                        --  Place "others" to the end of the list
                        Names.Insert
                          (Wide_Wide_Character'Last & "others", C, Dummy);

                     else
                        declare
                           Local : Sets.Set;
                        begin
                           for Identifier of Alternative_List.Children
                             when not Identifier.Is_Null
                           loop
                              Local.Insert (Identifier.Text, C, Dummy);

                              if C /= Local.Last then
                                 Location := Node.Sloc_Range.Start_Sloc;
                                 return True;
                              end if;
                           end loop;

                           Names.Insert (Local.First_Element, C, Dummy);
                           if C /= Names.Last then
                              Location := Node.Sloc_Range.Start_Sloc;
                              return True;
                           end if;
                        end;
                     end if;
                  end if;
               end loop;
            end loop;
         end if;
      end loop;

      return False;
   exception
      when E : others =>
         Refactor_Trace.Trace
           (E,
            LAL_Refactor.Is_Refactoring_Tool_Available_Default_Error_Message
              (Tool_Name));
         return False;
   end Is_Sort_Alphabetically_Available;

   --------------------
   -- Get_Delaration --
   --------------------

   procedure Get_Delaration
     (Node   : Ada_Node;
      Vector : out Text_Vectors.Vector)
   is
      Name_Node     : Name;
      Definition    : Defining_Name;
      Type_Expr     : Libadalang.Analysis.Type_Expr;
      Concrete_Type : Ada_Node;
      Enum_List     : Ada_Node;

      Dummy         : Ref_Result_Kind;
   begin
      --  Name of the variable
      Name_Node := Laltools.Common.Get_Node_As_Name (Node);
      if Name_Node.Is_Null then
         return;
      end if;

      --  Definition of the variable
      Definition := Laltools.Common.Resolve_Name
        (Name_Node, null, Dummy);
      if Definition.Is_Null then
         return;
      end if;

      --  Variable type expression
      Type_Expr := Definition.P_Basic_Decl.P_Type_Expression;
      if Type_Expr.Is_Null then
         return;
      end if;

      --  Definition of the type
      Definition := Laltools.Common.Resolve_Name
        (Type_Expr.P_Type_Name, null, Dummy);

      Concrete_Type := Find_Parent
        (Definition.As_Ada_Node, Is_Concrete_Type_Decl'Access);

      if Concrete_Type.Is_Null
        or else Concrete_Type.Kind /= Ada_Concrete_Type_Decl
      then
         return;
      end if;

      --  Concrete_Type is Ada_Concrete_Type_Decl,
      --   looking for Ada_Enum_Type_Def
      for Ch of Concrete_Type.Children when not Ch.Is_Null loop
         if Ch.Kind = Ada_Enum_Type_Def then
            --  Enum_List is should be Ada_Enum_Literal_Decl_List
            Enum_List := Ch.Child (Ch.First_Child_Index);

            if Enum_List.Kind /= Ada_Enum_Literal_Decl_List then
               return;
            end if;

            --  Fill literals in the order as they are declared
            --  Literal is Ada_Enum_Literal_Decl
            for Literal of Enum_List.Children when not Literal.Is_Null loop
               Vector.Append (Literal.Text);
            end loop;
         end if;
      end loop;
   end Get_Delaration;

   -----------------------------------
   -- Is_Sort_Declaration_Available --
   -----------------------------------

   function Is_Sort_Declaration_Available
     (Unit     : Analysis_Unit;
      Location : in out Source_Location)
      return Boolean
   is
      Token : constant Token_Reference := Unit.Lookup_Token (Location);
      Node  : constant Ada_Node        := Find_Parent
        (Lookup (Unit, Token, Forward),
         Is_Case'Access);

      Declaration : Text_Vectors.Vector;
      Cursor      : Text_Vectors.Cursor;

      function Check (T : Langkit_Support.Text.Text_Type) return Boolean;
      --  Returns True if T is not in declaration's order

      -----------
      -- Check --
      -----------

      function Check (T : Langkit_Support.Text.Text_Type) return Boolean
      is
         use Text_Vectors;
      begin
         while Element (Cursor) /= T loop
            Next (Cursor);

            if Cursor = No_Element then
               --  The end of the declaration list but we did not find the
               --  literal, so it was skipped before when we were looking
               --  for another lineral, which means that
               --  we have the wrong order
               Location := Node.Sloc_Range.Start_Sloc;
               return True;
            end if;
         end loop;

         return False;
      end Check;

   begin
      if Node.Is_Null then
         return False;
      end if;

      --  Node is CaseStmt
      for Aux of Node.Children when not Aux.Is_Null loop
         if Aux.Kind = Ada_Identifier then
            --  Aux is the name of the variable that case checks
            Get_Delaration (Aux, Declaration);

            if Declaration.Is_Empty then
               --  Did not find enumerate type literals
               return False;
            end if;
            Cursor := Declaration.First;

         elsif Aux.Kind = Ada_Case_Stmt_Alternative_List then
            --  Aux is the list of `when`es
            for Case_Stmt_Alternative of Aux.Children
              when not Case_Stmt_Alternative.Is_Null
            loop
               --  Case_Stmt_Alternative is one `when .. =>`
               for Alternatives_List of Case_Stmt_Alternative.Children
                 when not Alternatives_List.Is_Null
               loop
                  if Alternatives_List.Kind = Ada_Alternatives_List then
                     --  Alternatives_List is the list of literals in
                     --  the `when .. =>`
                     for Identifier of Alternatives_List.Children
                       when not Identifier.Is_Null
                     loop
                        --  Identifier each literal in the when list
                        if Identifier.Kind = Ada_Bin_Op then
                           --  `when Literal1 .. Literal2 =>` case
                           for Literal of Identifier.Children
                             when not Literal.Is_Null
                           loop
                              --  Looking for the firts literal
                              if Literal.Kind = Ada_Identifier then
                                 if Check (Literal.Text) then
                                    return True;
                                 end if;
                                 exit;
                              end if;
                           end loop;

                        elsif Identifier.Kind /= Ada_Others_Designator then
                           --  not `when others =>` case
                           if Check (Identifier.Text) then
                              return True;
                           end if;
                        end if;
                     end loop;
                  end if;
               end loop;
            end loop;
         end if;
      end loop;

      return False;
   exception
      when E : others =>
         Refactor_Trace.Trace
           (E,
            LAL_Refactor.Is_Refactoring_Tool_Available_Default_Error_Message
              (Tool_Name));
         return False;
   end Is_Sort_Declaration_Available;

   -------------------------------------
   -- Create_Alphabetical_Case_Sorter --
   -------------------------------------

   function Create_Alphabetical_Case_Sorter
     (Unit     : Analysis_Unit;
      Location : Source_Location)
      return Alphabetical_Case_Sorter
   is
      Token : constant Token_Reference := Unit.Lookup_Token (Location);
      Node  : constant Ada_Node        := Lookup (Unit, Token, Forward);
   begin
      for Aux of Node.Children when not Aux.Is_Null loop
         if Aux.Kind = Ada_Case_Stmt_Alternative_List then
            return Alphabetical_Case_Sorter'(Unit, Aux);
         end if;
      end loop;

      return Alphabetical_Case_Sorter'(Unit, No_Ada_Node);
   end Create_Alphabetical_Case_Sorter;

   ------------------------------------
   -- Create_Declaration_Case_Sorter --
   ------------------------------------

   function Create_Declaration_Case_Sorter
     (Unit     : Analysis_Unit;
      Location : Source_Location)
      return Declaration_Case_Sorter
   is
      Token : constant Token_Reference := Unit.Lookup_Token (Location);
      Node  : constant Ada_Node        := Lookup (Unit, Token, Forward);
   begin
      return Declaration_Case_Sorter'(Unit, Node);
   end Create_Declaration_Case_Sorter;

   --------------
   -- Refactor --
   --------------

   overriding
   function Refactor
     (Self           : Alphabetical_Case_Sorter;
      Analysis_Units : access function return Analysis_Unit_Array)
      return Refactoring_Edits
   is
      use Sets; use Maps;

      Whenes            : Maps.Map;
      Whenes_Cursor     : Maps.Cursor;
      Whenes_Reorder    : Boolean := False;
      Alternatives      : Ada_Node;

      Whenes_Text_Edits : Text_Edit_Ordered_Set;
      Names_Text_Edits  : Text_Edit_Ordered_Set;
      Dummy             : Boolean;

      Result            : Refactoring_Edits;
   begin
      --  iterate over Ada_Case_Stmt_Alternative_List children
      for Aux of Self.Node.Children when not Aux.Is_Null loop
         --  Aux is CaseStmtAlternative

         declare
            Names        : Sets.Set;
            Names_Cursor : Sets.Cursor;
            Reorder      : Boolean := False;
         begin
            for Current of Aux.Children when not Current.Is_Null loop
               --  Looking for AlternativesList
               if Current.Kind = Ada_Alternatives_List then
                  Alternatives := Current;

                  --  Iterate over Identifiers
                  for Identifier of Alternatives.Children
                    when not Identifier.Is_Null
                  loop
                     if Identifier.Kind = Ada_Others_Designator then
                        --  Place "others" to the end of the case's "where"s
                        Names.Insert
                          (Wide_Wide_Character'Last & "others",
                           Names_Cursor, Dummy);

                     else
                        Names.Insert (Identifier.Text, Names_Cursor, Dummy);

                        if Names_Cursor /= Names.Last then
                           Reorder := True;
                        end if;
                     end if;
                  end loop;

                  exit;
               end if;
            end loop;

            if Reorder then
               declare
                  use Ada.Strings.Wide_Wide_Unbounded;

                  Start : constant Natural := Aux.Text'First - 1;
                  Text  : Unbounded_Wide_Wide_String :=
                    To_Unbounded_Wide_Wide_String (Aux.Text);

               begin
                  Names_Cursor := Names.Last;
                  for Identifier of reverse Alternatives.Children
                    when not Identifier.Is_Null
                  loop
                     --  Edit for reordering identifiers in case we don't need
                     --  to reorder "where"s
                     Names_Text_Edits.Insert
                       ((Identifier.Sloc_Range,
                        To_Unbounded_String
                          (Langkit_Support.Text.To_UTF8
                             (Names.Element (Names_Cursor)))));

                     --  "where" text with reordered identifiers
                     declare
                        I : constant Langkit_Support.Text.Text_Type :=
                          Identifier.Text;
                     begin
                        Text.Replace_Slice
                          (I'First - Start,
                           I'Last - Start,
                           Names.Element (Names_Cursor));
                     end;

                     Previous (Names_Cursor);
                  end loop;

                  --  First identifier + whole "when" text with
                  --  reordered identifiers
                  Whenes.Insert
                    (Names.First_Element,
                     Text.To_Wide_Wide_String, Whenes_Cursor, Dummy);
               end;

            else
               --  Identifier + whole "when" text
               Whenes.Insert
                 (Names.First_Element, Aux.Text, Whenes_Cursor, Dummy);
            end if;

            if Whenes_Cursor /= Whenes.Last then
               Whenes_Reorder := True;
            end if;
         end;
      end loop;

      if Whenes_Reorder then
         --  We need to reorder 'when's
         Whenes_Cursor := Whenes.First;
         for Aux of Self.Node.Children when not Aux.Is_Null loop
            Whenes_Text_Edits.Insert
              ((Aux.Sloc_Range,
               To_Unbounded_String
                 (Langkit_Support.Text.To_UTF8 (Element (Whenes_Cursor)))));

            Next (Whenes_Cursor);
         end loop;

         Result.Text_Edits.Insert (Self.Unit.Get_Filename, Whenes_Text_Edits);

      else
         --  We need to reorder identifiers in some "when" only
         Result.Text_Edits.Insert (Self.Unit.Get_Filename, Names_Text_Edits);
      end if;

      return Result;

   exception
      when E : others =>
         Refactor_Trace.Trace
           (E,
            Refactoring_Tool_Refactor_Default_Error_Message (Tool_Name));

         return No_Refactoring_Edits;
   end Refactor;

   --------------
   -- Refactor --
   --------------

   overriding
   function Refactor
     (Self           : Declaration_Case_Sorter;
      Analysis_Units : access function return Analysis_Unit_Array)
      return Refactoring_Edits
   is
      use Ada.Strings.Wide_Wide_Unbounded;
      use type Ada.Containers.Count_Type;

      Declaration       : Text_Vectors.Vector;
      Whenes            : Literal_Vectors.Vector;

      L, R              : Natural := 0;
      Whenes_Text_Edits : Text_Edit_Ordered_Set;
      Names_Text_Edits  : Text_Edit_Ordered_Set;
      Result            : Refactoring_Edits;
   begin
      --  Node is CaseStmt
      for N of Self.Node.Children when not N.Is_Null loop
         if N.Kind = Ada_Identifier then
            Get_Delaration (N, Declaration);

            if Declaration.Is_Empty then
               --  Did not find enumerate type literals
               return Result;
            end if;

         elsif N.Kind = Ada_Case_Stmt_Alternative_List then
            for Aux of N.Children when not Aux.Is_Null loop
               --  Aux is CaseStmtAlternative

               declare
                  Literals   : Literal_Vectors.Vector;
                  Start      : constant Natural := Aux.Text'First - 1;
                  Text       : Unbounded_Wide_Wide_String :=
                    To_Unbounded_Wide_Wide_String (Aux.Text);
                  First_Name : Unbounded_Wide_Wide_String;

                  procedure Append
                    (Name : Langkit_Support.Text.Text_Type;
                     Node : Ada_Node);

                  procedure Append
                    (Name : Langkit_Support.Text.Text_Type;
                     Node : Ada_Node)
                  is
                     Txt : constant Langkit_Support.Text.Text_Type :=
                       Node.Text;
                  begin
                     Literals.Append
                       (Literal_Info'
                          (To_Unbounded_Wide_Wide_String (Name),
                           To_Unbounded_Wide_Wide_String (Txt),
                           Txt'First,
                           Txt'Last,
                           Node.Sloc_Range));
                  end Append;

               begin
                  for Alternatives of Aux.Children
                    when not Alternatives.Is_Null
                  loop
                     --  Looking for AlternativesList
                     if Alternatives.Kind = Ada_Alternatives_List then
                        --  Iterate over Identifiers in when ... =>
                        for Identifier of Alternatives.Children
                          when not Identifier.Is_Null
                        loop
                           if Identifier.Kind = Ada_Others_Designator then
                              Append ("others", Identifier);

                           elsif Identifier.Kind = Ada_Bin_Op then
                              for Literal of Identifier.Children
                                when not Literal.Is_Null
                              loop
                                 if Literal.Kind = Ada_Identifier then
                                    --  use the firts literal from range A .. B
                                    Append (Literal.Text, Identifier);
                                    exit;
                                 end if;
                              end loop;

                           else
                              Append (Identifier.Text, Identifier);
                           end if;
                        end loop;

                        First_Name := Literals.First_Element.Name;

                        --  check we need reorder literals inside the `when`
                        if Literals.Length > 1 then
                           L := Literals.Last_Index;

                           for Literal of reverse Declaration loop
                              R := Literals.Last_Index;

                              while R >= Literals.First_Index
                                and then Literals.Element (R).Name /= Literal
                              loop
                                 R := R - 1;
                              end loop;

                              if R >= Literals.First_Index then
                                 --  found declaration literal in
                                 --   the `when` list
                                 First_Name := Literals.Element (R).Name;

                                 if L /= R then
                                    --  Literal should be moved from R to L
                                    Names_Text_Edits.Insert
                                      ((Literals.Element (L).SLOC,
                                       To_Unbounded_String
                                         (Langkit_Support.Text.To_UTF8
                                            (To_Wide_Wide_String
                                                 (Literals.Element
                                                      (R).Text)))));

                                    Text.Replace_Slice
                                      (Literals.Element (L).Text_First - Start,
                                       Literals.Element (L).Text_Last - Start,
                                       To_Wide_Wide_String
                                         (Literals.Element (R).Text));
                                 end if;

                                 exit when L = Literals.First_Index;
                                 L := L - 1;
                              end if;
                           end loop;
                        end if;

                        exit;
                     end if;
                  end loop;

                  Whenes.Append
                    (Literal_Info'(First_Name, Text, 0, 0, Aux.Sloc_Range));
               end;
            end loop;
         end if;
      end loop;

      --  check we need to reorder `when`es
      L := Whenes.First_Index;
      for Literal of Declaration loop
         R := Whenes.First_Index;

         while R <= Whenes.Last_Index
           and then Whenes.Element (R).Name /= Literal
         loop
            R := R + 1;
         end loop;

         if R <= Whenes.Last_Index then
            --  found declaration literal in
            --   the `when` list
            if L /= R then
               --  Literal should be moved from R to L
               Whenes_Text_Edits.Insert
                 ((Whenes.Element (L).SLOC,
                  To_Unbounded_String
                    (Langkit_Support.Text.To_UTF8
                       (To_Wide_Wide_String (Whenes.Element (R).Text)))));
            end if;

            exit when L = Whenes.Last_Index;
            L := L + 1;
         end if;
      end loop;

      if not Whenes_Text_Edits.Is_Empty then
         Result.Text_Edits.Insert (Self.Unit.Get_Filename, Whenes_Text_Edits);

      elsif not Names_Text_Edits.Is_Empty then
         Result.Text_Edits.Insert (Self.Unit.Get_Filename, Names_Text_Edits);
      end if;

      return Result;
   exception
      when E : others =>
         Refactor_Trace.Trace
           (E,
            Refactoring_Tool_Refactor_Default_Error_Message (Tool_Name));

         return No_Refactoring_Edits;
   end Refactor;

end LAL_Refactor.Sort_Case;
