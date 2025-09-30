--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Langkit_Support.Text;

with Libadalang.Common;  use Libadalang.Common;

with LAL_Refactor.Tools; use LAL_Refactor.Tools;

package body LAL_Refactor.Inline_Variable is

   Tool_Name : constant String := "Inline Variable";

   function Is_Decl (Node : Ada_Node'Class) return Boolean is
     (not Node.Is_Null and then Node.Kind in Ada_Object_Decl);

   function Is_Defining (Node : Ada_Node'Class) return Boolean is
     (not Node.Is_Null and then Node.Kind in Ada_Defining_Name);

   ----------------------------------
   -- Is_Inline_Variable_Available --
   ----------------------------------

   function Is_Inline_Variable_Available
     (Unit           : Analysis_Unit;
      Location       : Source_Location;
      Analysis_Units : access function return Analysis_Unit_Array)
      return Boolean
   is
      Node : constant Ada_Node := Find_Parent
        (Lookup
           (Unit, Unit.Lookup_Token (Location), Forward),
         Is_Decl'Access);

      Def  : constant Ada_Node := Find_Parent
        (Lookup
           (Unit, Unit.Lookup_Token (Location), Forward),
         Is_Defining'Access);

      Defining       : Defining_Name;
      Is_Subtype     : Boolean := False;
      Is_Expression  : Boolean := False;
   begin
      if Node.Is_Null
        or else Def.Is_Null
      then
         return False;
      end if;

      Defining := Def.As_Defining_Name;
      if Defining.Is_Null then
         return False;
      end if;

      for Child of Node.Children loop
         if not Child.Is_Null then
            if Child.Kind = Ada_Subtype_Indication then
               Is_Subtype := True;

            elsif Is_Subtype then
               Is_Expression := True;
            end if;
         end if;
      end loop;

      if not Is_Expression then
         --  Do not have initialization expression
         return False;
      end if;

      for Item of P_Find_All_References (Defining, Analysis_Units.all) loop
         if Ref (Item).P_Is_Write_Reference then
            --  Do not allow action if we have assign to the variable
            --  somewhere in code
            return False;
         end if;
      end loop;

      return True;

   exception
      when E : others =>
         Refactor_Trace.Trace
           (E,
            LAL_Refactor.Is_Refactoring_Tool_Available_Default_Error_Message
              (Tool_Name));
         return False;
   end Is_Inline_Variable_Available;

   -----------------------------
   -- Create_Variable_Inliner --
   -----------------------------

   function Create_Variable_Inliner
     (Unit     : Analysis_Unit;
      Location : Source_Location)
      return Variable_Inliner
   is
      Node : constant Ada_Node := Find_Parent
        (Lookup
           (Unit, Unit.Lookup_Token (Location), Forward),
         Is_Decl'Access);

   begin
      return Variable_Inliner'(Unit, Node, Location);
   end Create_Variable_Inliner;

   --------------
   -- Refactor --
   --------------

   function Refactor
     (Self           : Variable_Inliner;
      Analysis_Units : access function return Analysis_Unit_Array)
      return Refactoring_Edits
   is
      Defining       : constant Defining_Name := Find_Parent
        (Lookup
           (Self.Unit, Self.Unit.Lookup_Token (Self.Location), Forward),
         Is_Defining'Access).As_Defining_Name;

      Start_Line     : Line_Number;
      Start_Column   : Column_Number;
      End_Line       : Line_Number;
      End_Column     : Column_Number;

      Edits          : Refactoring_Edits;
      Defining_Count : Natural := 0;

      Expr         : Unbounded_String;
      Is_Subtype   : Boolean := False;
   begin
      for Child of Self.Node.Children loop
         if not Child.Is_Null then
            if Child.Kind = Ada_Defining_Name_List then
               for K of Child.Children loop
                  if not K.Is_Null
                    and then K.Kind = Ada_Defining_Name
                  then
                     Defining_Count := Defining_Count + 1;
                  end if;
               end loop;

            elsif Child.Kind = Ada_Subtype_Indication then
               Is_Subtype := True;

            elsif Is_Subtype then
               Expr := To_Unbounded_String
                 (Langkit_Support.Text.To_UTF8 (Child.Text));

               if Child.Kind = Ada_Bin_Op then
                  Expr := "(" & Expr & ")";
               end if;
            end if;
         end if;
      end loop;

      declare
         Text_Edits : Text_Edit_Ordered_Set;
         Token      : Token_Reference;

      begin
         if Defining_Count = 1 then
            End_Line   := Self.Node.Sloc_Range.End_Line;
            End_Column := Self.Node.Sloc_Range.End_Column;

            Token := Next_Non_Whitespace
              (Previous (Self.Node.Token_Start), Backward);

            if Token /= No_Token then
               Start_Line   := Token.Data.Sloc_Range.End_Line;
               Start_Column := Token.Data.Sloc_Range.End_Column + 1;
            end if;

         else
            Start_Line   := Defining.Sloc_Range.Start_Line;
            Start_Column := Defining.Sloc_Range.Start_Column;
            End_Line     := Defining.Sloc_Range.End_Line;
            End_Column   := Defining.Sloc_Range.End_Column;

            Token := Next_Non_Whitespace (Defining.Token_End, Forward);

            if Token /= No_Token
              and then Kind (Token.Data) = Ada_Comma
            then
               Token      := Next_Non_Whitespace (Token, Forward);
               End_Line   := Token.Data.Sloc_Range.Start_Line;
               End_Column := Token.Data.Sloc_Range.Start_Column;

            else
               Token := Next_Non_Whitespace (Defining.Token_Start, Backward);

               if Token /= No_Token
                 and then Kind (Token.Data) = Ada_Comma
               then
                  Token        := Next_Non_Whitespace (Token, Backward);
                  Start_Line   := Token.Data.Sloc_Range.End_Line;
                  Start_Column := Token.Data.Sloc_Range.End_Column;
               end if;
            end if;
         end if;

         Text_Edits.Insert
           (((Start_Line, End_Line, Start_Column, End_Column),
            Null_Unbounded_String));
         Edits.Text_Edits.Insert (Self.Node.Unit.Get_Filename, Text_Edits);
      end;

      for Item of P_Find_All_References (Defining, Analysis_Units.all) loop
         Replace_Node (Edits.Text_Edits, Ref (Item).As_Ada_Node, Expr);
      end loop;

      return Edits;

   exception
      when E : others =>
         Refactor_Trace.Trace
           (E,
            Refactoring_Tool_Refactor_Default_Error_Message (Tool_Name));

         return No_Refactoring_Edits;
   end Refactor;

end LAL_Refactor.Inline_Variable;
