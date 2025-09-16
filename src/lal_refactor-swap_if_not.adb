--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Strings.Fixed;
with Langkit_Support.Text;

with Libadalang.Common;  use Libadalang.Common;

with LAL_Refactor.Tools; use LAL_Refactor.Tools;

package body LAL_Refactor.Swap_If_Not is

   Tool_Name : constant String := "Swap If Not";

   function Is_If (Node : Ada_Node'Class) return Boolean is
     (not Node.Is_Null and then Node.Kind in Ada_If_Stmt);

   -----------------------
   -- Is_Swap_Available --
   -----------------------

   function Is_Swap_Available
     (Unit     : Analysis_Unit;
      Location : in out Source_Location)
      return Boolean
   is
      Node : constant Ada_Node := Find_Parent
        (Lookup
           (Unit,
            Unit.Lookup_Token (Location),
            Forward),
         Is_If'Access);

      Is_Elsif, Is_Else : Boolean := False;
   begin
      if Node.Is_Null then
         return False;
      end if;

      for Aux of Node.Children loop
         if not Aux.Is_Null then
            if Aux.Kind = Ada_Elsif_Stmt_Part_List then
               Is_Elsif := Aux.Children_Count /= 0;

            elsif Aux.Kind = Ada_Else_Part then
               Is_Else := Aux.Children_Count /= 0;
            end if;
         end if;
      end loop;

      if Is_Else
        and then not Is_Elsif
      then
         Location :=
           (Line   => Node.Sloc_Range.Start_Sloc.Line,
            Column => Node.Sloc_Range.Start_Sloc.Column);

         return True;
      else
         return False;
      end if;

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
      return Swaper'
        (Unit,
         Lookup (Unit, Unit.Lookup_Token (Location), Forward));
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

      Text_Edits : Text_Edit_Ordered_Set;
      Edits      : Refactoring_Edits;

      If_Txt     : Unbounded_String;
      If_Loc     : Source_Location_Range;
      Else_Txt   : Unbounded_String;
      Else_Loc   : Source_Location_Range;

      Token      : Token_Reference;

   begin
      for Aux of Self.Node.Children loop
         if not Aux.Is_Null then
            if Text_Edits.Length = 0
              and then Aux.Kind = Ada_Un_Op
            then
               --  first simple statment, check whether is `not` statment
               for N of Aux.Children loop
                  if not N.Is_Null then
                     if Text_Edits.Length = 0
                       and then N.Kind = Ada_Op_Not
                     then
                        --  It is `not` statment, delete `not`
                        Token := Self.Unit.Lookup_Token
                          ((N.Sloc_Range.Start_Line,
                           N.Sloc_Range.Start_Column - 1));

                        if Token /= No_Token
                          and then Token.Data.Kind = Ada_Whitespace
                        then
                           --  Delete whitespace before the `not`
                           Text_Edits.Insert
                             ((Location => (N.Sloc_Range.Start_Line,
                                            N.Sloc_Range.End_Line,
                                            N.Sloc_Range.Start_Column - 1,
                                            N.Sloc_Range.End_Column),
                               Text     => Null_Unbounded_String));
                        else
                           Token := Self.Unit.Lookup_Token
                             ((N.Sloc_Range.End_Line,
                              N.Sloc_Range.End_Column));
                           if Token /= No_Token
                             and then Token.Data.Kind = Ada_Whitespace
                           then
                              --  Delete whitespace after the `not`
                              Text_Edits.Insert
                                ((Location => (N.Sloc_Range.Start_Line,
                                               N.Sloc_Range.End_Line,
                                               N.Sloc_Range.Start_Column,
                                               N.Sloc_Range.End_Column + 1),
                                  Text     => Null_Unbounded_String));
                           else
                              --  Did not find the whitespace before and
                              --  after the `not`, just delete `not` itself
                              Text_Edits.Insert
                                ((Location => (N.Sloc_Range.Start_Line,
                                               N.Sloc_Range.End_Line,
                                               N.Sloc_Range.Start_Column,
                                               N.Sloc_Range.End_Column),
                                  Text     => Null_Unbounded_String));
                           end if;
                        end if;

                     elsif Text_Edits.Length > 0
                       and then N.Kind = Ada_Paren_Expr
                     then
                        --  `not` is deleted, also trim `(` `)`
                        declare
                           T : constant String :=
                             Langkit_Support.Text.To_UTF8 (N.Text);
                        begin
                           Text_Edits.Insert
                             ((Location => N.Sloc_Range,
                               Text     => To_Unbounded_String
                                 (Ada.Strings.Fixed.Trim
                                    (T (T'First + 1 .. T'Last - 1),
                                     Ada.Strings.Both))));
                        end;
                     end if;
                  end if;
               end loop;

               if Text_Edits.Length = 0 then
                  --  `not` not found,
                  --  so conwert simple statment to `not stament`
                  Text_Edits.Insert
                    ((Location => (Aux.Sloc_Range.Start_Line,
                                   Aux.Sloc_Range.Start_Line,
                                   Aux.Sloc_Range.Start_Column,
                                   Aux.Sloc_Range.Start_Column),
                      Text     => (To_Unbounded_String ("not "))));
               end if;

            elsif Text_Edits.Length = 0
              and then Aux.Kind in Ada_Expr
            then
               --  first complex statment, convert to `not (statment)`

               Token := Self.Unit.Lookup_Token
                 ((Aux.Sloc_Range.Start_Line,
                  Aux.Sloc_Range.Start_Column - 1));

               Text_Edits.Insert
                 ((Location => (Aux.Sloc_Range.Start_Line,
                                Aux.Sloc_Range.End_Line,
                                Aux.Sloc_Range.Start_Column,
                                Aux.Sloc_Range.End_Column),
                   Text     =>
                     (To_Unbounded_String
                        (if Token /= No_Token
                         and then Token.Data.Kind = Ada_Whitespace
                         then ""
                         else " ") & "not (" &
                        Langkit_Support.Text.To_UTF8 (Aux.Text) & ")")));

            elsif Aux.Kind = Ada_Stmt_List then
               If_Txt := To_Unbounded_String
                 (Langkit_Support.Text.To_UTF8 (Aux.Text));
               If_Loc := Aux.Sloc_Range;

            elsif Aux.Kind = Ada_Else_Part then
               for N of Aux.Children loop
                  if N.Kind = Ada_Stmt_List then
                     Else_Txt := To_Unbounded_String
                       (Langkit_Support.Text.To_UTF8 (N.Text));
                     Else_Loc := N.Sloc_Range;
                  end if;
               end loop;
            end if;
         end if;
      end loop;

      --  Swap: if <-> else
      Text_Edits.Insert
        ((Location => If_Loc,
          Text     => Else_Txt));

      Text_Edits.Insert
        ((Location => Else_Loc,
          Text     => If_Txt));

      Edits.Text_Edits.Insert (Self.Unit.Get_Filename, Text_Edits);
      return Edits;

   exception
      when E : others =>
         Refactor_Trace.Trace
           (E,
            Refactoring_Tool_Refactor_Default_Error_Message (Tool_Name));

         return No_Refactoring_Edits;
   end Refactor;

end LAL_Refactor.Swap_If_Not;
