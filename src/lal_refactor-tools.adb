--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Characters.Handling;
with Ada.Command_Line;
with Ada.Containers.Indefinite_Hashed_Sets;
with Ada.Strings.Hash;

package body LAL_Refactor.Tools is

   -------------
   -- Convert --
   -------------

   function Convert (Arg : String) return Tool is
   begin
      return Tool'Value (Arg);
   exception
      when others =>
         raise Parse_Tool_Exception;
   end Convert;

   ------------
   -- Lookup --
   ------------

   function Lookup
     (Unit  : Analysis_Unit;
      Token : in out Token_Reference;
      Going : Direction;
      Skip  : Token_Kinds)
      return Ada_Node
   is
      Aux_Token      : Token_Reference := Token;
      Aux_Token_Kind : Token_Kind      := Kind (Data (Aux_Token));

      -----------------
      -- Should_Skip --
      -----------------

      function Should_Skip (Kind : Token_Kind) return Boolean;
      function Should_Skip (Kind : Token_Kind) return Boolean is
      begin
         for K of Skip loop
            if K = Kind then
               return True;
            end if;
         end loop;

         return False;
      end Should_Skip;

   begin
      --  Do nothing if Aux_Token <=> Token is a No_Token or already belongs to
      --  an Ada_Node.

      while not (Aux_Token = No_Token)
        and then Should_Skip (Aux_Token_Kind)
      loop
         case Going is
            when Forward => Aux_Token := Next (Aux_Token);
            when Backward => Aux_Token := Previous (Aux_Token);
         end case;

         Aux_Token_Kind := Kind (Data (Aux_Token));
      end loop;

      --  No Ada_Node was found relative to Token

      if Aux_Token = No_Token then
         return No_Ada_Node;
      end if;

      Token := Aux_Token;
      return Unit.Root.Lookup (Start_Sloc (Sloc_Range (Data (Aux_Token))));
   end Lookup;

   ------------
   -- Lookup --
   ------------

   function Lookup
     (Unit  : Analysis_Unit;
      Token : Token_Reference;
      Going : Direction)
      return Ada_Node
   is
      Aux_Token : Token_Reference := Token;
   begin
      return Lookup
        (Unit, Aux_Token, Going, (Ada_Comment, Ada_Whitespace));
   end Lookup;

   ----------
   -- Find --
   ----------

   function Find
     (Token : Token_Reference;
      Kind  : Token_Kind;
      Going : Direction)
      return Token_Reference
   is
      Aux_Token      : Token_Reference := Token;
      Aux_Token_Kind : Token_Kind      :=
        Libadalang.Common.Kind (Data (Aux_Token));

   begin
      while Aux_Token /= No_Token
        and then Aux_Token_Kind /= Kind
      loop
         case Going is
            when Forward => Aux_Token := Next (Aux_Token);
            when Backward => Aux_Token := Previous (Aux_Token);
         end case;

         Aux_Token_Kind := Libadalang.Common.Kind (Data (Aux_Token));
      end loop;

      return Aux_Token;
   end Find;

   -------------------------
   -- Next_Non_Whitespace --
   -------------------------

   function Next_Non_Whitespace
     (Token : Token_Reference;
      Going : Direction)
      return Token_Reference
   is
      Result : Token_Reference := Token;

   begin
      --  Do nothing if Result <=> Token is a No_Token

      while Result /= No_Token loop
         case Going is
            when Forward => Result := Next (Result);
            when Backward => Result := Previous (Result);
         end case;

         exit when Kind (Data (Result)) /= Ada_Whitespace;
      end loop;

      if Result /= No_Token
         and then Kind (Data (Result)) /= Ada_Whitespace
      then
         --  No whitespace relative to Token
         return Result;

      else
         return No_Token;
      end if;
   end Next_Non_Whitespace;

   ---------------
   -- Tool_List --
   ---------------

   function Tool_List return String is
      use Ada.Characters.Handling;
      H : Unbounded_String;
   begin
      Append (H, To_Lower (Tool'Image (Tool'First)));
      --  if Tool'Range_Length = 1 then
      --     Append (H, To_Lower (Tool'Image (Tool'First)));
      --  else
      --     Append (H, To_Lower (Tool'Image (Tool'First)));
      --     for J in Tool'Succ (Tool'First) .. Tool'Last loop
      --        Append (H, (LF & "         " & To_Lower (Tool'Image (J))));
      --     end loop;
      --  end if;
      return To_String (H);
   end Tool_List;

   ---------------------------
   -- Find_First_Tool_Index --
   ---------------------------

   function Find_First_Tool_Index return Natural
   is
      package String_Hashed_Sets is new Ada.Containers.Indefinite_Hashed_Sets
        (Element_Type        => String,
         Hash                => Ada.Strings.Hash,
         Equivalent_Elements => "=");

      Tools_Set : String_Hashed_Sets.Set;

      use Ada.Characters.Handling;
      use Ada.Command_Line;

   begin
      for J in Tool'First .. Tool'Last loop
         Tools_Set.Insert (To_Lower (Tool'Image (J)));
      end loop;

      for J in 1 .. Argument_Count loop
         if Tools_Set.Contains (To_Lower (Argument (J))) then
            return J;
         end if;
      end loop;

      return 0;
   end Find_First_Tool_Index;

end LAL_Refactor.Tools;
