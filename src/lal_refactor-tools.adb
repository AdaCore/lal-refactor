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
