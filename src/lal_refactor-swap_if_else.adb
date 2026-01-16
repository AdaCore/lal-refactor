--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Strings.Fixed;
with Langkit_Support.Text;

with Libadalang.Common;  use Libadalang.Common;

with LAL_Refactor.Tools; use LAL_Refactor.Tools;

package body LAL_Refactor.Swap_If_Else is

   Tool_Name : constant String := "Swap If Else";

   function Is_If (Node : Ada_Node'Class) return Boolean is
     (not Node.Is_Null and then Node.Kind in Ada_If_Stmt);

   function Find_Node (Node : Ada_Node'Class) return Boolean is
     (not Node.Is_Null and then
        (Node.Kind in Ada_If_Stmt
         or else Node.Kind in Ada_Elsif_Stmt_Part
         or else Node.Kind in Ada_Else_Part));

   -----------------------
   -- Is_Swap_Available --
   -----------------------

   function Is_Swap_Available
     (Unit     : Analysis_Unit;
      Location : Source_Location)
      return Boolean
   is
      Node : constant Ada_Node := Find_Parent
        (Lookup
           (Unit,
            Unit.Lookup_Token (Location),
            Forward),
         Is_If'Access);

      Last_Elsif : Source_Location_Range := No_Source_Location_Range;
   begin
      if Node.Is_Null then
         return False;
      end if;

      for Aux of Node.Children when not Aux.Is_Null loop
         if Aux.Kind = Ada_Elsif_Stmt_Part_List then
            --  Iterate over all `elsif` parts
            for Els of Aux.Children when not Els.Is_Null loop
               if Els.Kind = Ada_Elsif_Stmt_Part then
                  --  Store elsif's location range, Last_Elsif will
                  --  contain the last elsif's location range after exit
                  Last_Elsif := Els.Sloc_Range;
               end if;
            end loop;

         elsif Aux.Kind = Ada_Else_Part
           and then Aux.Children_Count /= 0
         then
            --  Allow if no elsif part or location is in the last elsif
            return Last_Elsif = No_Source_Location_Range
              or else Compare (Last_Elsif, Location) = Inside;
         end if;
      end loop;

      --  No else part, not allowed
      return False;

   exception
      when E : others =>
         Refactor_Trace.Trace
           (E,
            LAL_Refactor.Is_Refactoring_Tool_Available_Default_Error_Message
              (Tool_Name));
         return False;
   end Is_Swap_Available;

   -------------------
   -- Create_Swaper --
   -------------------

   function Create_Swaper
     (Unit     : Analysis_Unit;
      Location : Source_Location)
      return Swaper is
   begin
      return Swaper'(Unit, Location);
   end Create_Swaper;

   --------------
   -- Refactor --
   --------------

   function Refactor
     (Self           : Swaper;
      Analysis_Units : access function return Analysis_Unit_Array)
      return Refactoring_Edits
   is
      use type Ada.Containers.Count_Type;

      File      : constant String := Self.Unit.Get_Filename;

      Loc_Token : constant Token_Reference :=
        Self.Unit.Lookup_Token (Self.Location);
      Loc_Node  : constant Ada_Node := Find_Parent
        (Lookup (Self.Unit, Loc_Token, Forward), Find_Node'Access);
      If_Node   : constant Ada_Node := Find_Parent
        (Lookup (Self.Unit, Loc_Token, Forward), Is_If'Access);
      Iterate   : Ada_Node;

      If_Stmnt   : Ada_Node;
      Else_Stmnt : Ada_Node;
      Token      : Token_Reference;
      Token1     : Token_Reference;
      Result     : Refactoring_Edits;

   begin
      Result.Text_Edits.Insert (File, Text_Edit_Ordered_Sets.Empty_Set);

      Iterate := (if Loc_Node.Kind = Ada_Else_Part
                  then If_Node
                  else Loc_Node);

      Main : for Aux of Iterate.Children when not Aux.Is_Null
        --  iterate over the located node if it is not the else part
        --  (e.g. If itself or some Elsif) to reverse condition and
        --  get statments
      loop
         if Result.Text_Edits.Element (File).Length = 0
           and then Aux.Kind = Ada_Un_Op
         then
            for N of Aux.Children when not N.Is_Null loop
               --  first simple statment, check whether is `not` statment
               if Result.Text_Edits.Element (File).Length = 0
                 and then N.Kind = Ada_Op_Not
               then
                  --  It is `not` statment, delete `not`
                  Token := Previous (N.Token_Start);

                  if Token /= No_Token
                    and then Token.Data.Kind = Ada_Whitespace
                  then
                     --  Delete whitespace before the `not`
                     Token1 := N.Token_End;

                  else
                     --  Delete whitespace after the `not`
                     Token  := N.Token_Start;
                     Token1 := Next (N.Token_End);

                     if Token1 = No_Token
                       or else Token1.Data.Kind /= Ada_Whitespace
                     then
                        --  Did not find the whitespace before and
                        --  after the `not`, just delete `not` itself
                        Token1 := N.Token_End;
                     end if;
                  end if;

                  --  Delete `not`
                  Result.Text_Edits.Reference (File).Insert
                    ((Location => (Token.Data.Sloc_Range.Start_Line,
                                   Token1.Data.Sloc_Range.End_Line,
                                   Token.Data.Sloc_Range.Start_Column,
                                   Token1.Data.Sloc_Range.End_Column),
                      Text     => Null_Unbounded_String));

               elsif Result.Text_Edits.Element (File).Length > 0
               --  `not` is deleted
                 and then N.Kind = Ada_Paren_Expr
               then
                  --  `not` is deleted, also trim `(` `)`
                  declare
                     T : constant String :=
                       Langkit_Support.Text.To_UTF8 (N.Text);
                  begin
                     Result.Text_Edits.Reference (File).Insert
                       ((Location => N.Sloc_Range,
                         Text     => To_Unbounded_String
                           (Ada.Strings.Fixed.Trim
                              (T (T'First + 1 .. T'Last - 1),
                               Ada.Strings.Both))));
                  end;
               end if;
            end loop;

            if Result.Text_Edits.Element (File).Length = 0 then
               --  `not` not found,
               --  so convert this simple statment to `not statment`
               Result.Text_Edits.Reference (File).Insert
                 ((Location => (Aux.Sloc_Range.Start_Line,
                                Aux.Sloc_Range.Start_Line,
                                Aux.Sloc_Range.Start_Column,
                                Aux.Sloc_Range.Start_Column),
                   Text     => (To_Unbounded_String ("not "))));
            end if;

         elsif Result.Text_Edits.Element (File).Length = 0
           and then Aux.Kind in Ada_Relation_Op
         then
            --  reverse comparsion
            for N of Aux.Children when not N.Is_Null loop
               if N.Kind = Ada_Op_Eq then -- =
                  Replace_Node
                    (Result.Text_Edits, N, To_Unbounded_String ("/="));
                  exit;

               elsif N.Kind = Ada_Op_Neq then -- /=
                  Replace_Node
                    (Result.Text_Edits, N, To_Unbounded_String ("="));
                  exit;

               elsif N.Kind = Ada_Op_Gt then -- >
                  Replace_Node
                    (Result.Text_Edits, N, To_Unbounded_String ("<="));
                  exit;

               elsif N.Kind = Ada_Op_Lt then -- <
                  Replace_Node
                    (Result.Text_Edits, N, To_Unbounded_String (">="));
                  exit;

               elsif N.Kind = Ada_Op_Gte then -- >=
                  Replace_Node
                    (Result.Text_Edits, N, To_Unbounded_String ("<"));
                  exit;

               elsif N.Kind = Ada_Op_Lte then -- <=
                  Replace_Node
                    (Result.Text_Edits, N, To_Unbounded_String (">"));
                  exit;
               end if;
            end loop;

         elsif Result.Text_Edits.Element (File).Length = 0
           and then Aux.Kind in Ada_Expr
         then
            --  first complex statment, convert to `not (statment)`

            Token := Previous (Aux.Token_Start);

            Result.Text_Edits.Reference (File).Insert
              ((Location => Aux.Sloc_Range,
                Text     =>
                  (To_Unbounded_String
                     (if Token /= No_Token
                      and then Token.Data.Kind = Ada_Whitespace
                      then ""
                      else " ") & "not (" &
                     Langkit_Support.Text.To_UTF8 (Aux.Text) & ")")));

         elsif Aux.Kind = Ada_Stmt_List then
            If_Stmnt := Aux;

         elsif Aux.Kind = Ada_Else_Part then
            for N of Aux.Children when not N.Is_Null loop
               if N.Kind = Ada_Stmt_List then
                  Else_Stmnt := N;
                  exit Main;
               end if;
            end loop;
         end if;
      end loop Main;

      --  find else statments if not found yet
      if Else_Stmnt = No_Ada_Node then
         Find_Else : for Aux of If_Node.Children when not Aux.Is_Null loop
            if Aux.Kind = Ada_Else_Part then
               for N of Aux.Children when not N.Is_Null loop
                  if N.Kind = Ada_Stmt_List then
                     Else_Stmnt := N;
                     exit Find_Else;
                  end if;
               end loop;
            end if;
         end loop Find_Else;
      end if;

      --  Swap: if/elsif <-> else
      Result.Text_Edits.Reference (File).Insert
        ((Location => If_Stmnt.Sloc_Range,
          Text     => To_Unbounded_String
            (Langkit_Support.Text.To_UTF8 (Else_Stmnt.Text))));

      Result.Text_Edits.Reference (File).Insert
        ((Location => Else_Stmnt.Sloc_Range,
          Text     => To_Unbounded_String
            (Langkit_Support.Text.To_UTF8 (If_Stmnt.Text))));

      return Result;

   exception
      when E : others =>
         Refactor_Trace.Trace
           (E,
            Refactoring_Tool_Refactor_Default_Error_Message (Tool_Name));

         return No_Refactoring_Edits;
   end Refactor;

end LAL_Refactor.Swap_If_Else;
