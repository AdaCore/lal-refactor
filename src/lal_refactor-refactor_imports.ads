--
--  Copyright (C) 2020-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  This package contains the Refactor Imports Tool utilities

with Ada.Containers.Vectors;
with Ada.Strings.Wide_Wide_Unbounded;

with Langkit_Support.Text; use Langkit_Support.Text;

with Libadalang.Helpers; use Libadalang.Helpers;

package LAL_Refactor.Refactor_Imports is

   type Import_Suggestion is record
      Declaration      : Basic_Decl := No_Basic_Decl;
      With_Clause_Text : Unbounded_Text_Type :=
        Ada.Strings.Wide_Wide_Unbounded.Null_Unbounded_Wide_Wide_String;
      Prefix_Text      : Unbounded_Text_Type :=
        Ada.Strings.Wide_Wide_Unbounded.Null_Unbounded_Wide_Wide_String;
   end record;

   function "<" (Left, Right : Import_Suggestion) return Boolean;

   package Import_Suggestions_Vector is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Import_Suggestion);

   package Import_Suggestions_Vector_Sorting is new
     Import_Suggestions_Vector.Generic_Sorting;

   function Get_Import_Suggestions
     (Node  : Ada_Node'Class;
      Units : Unit_Vectors.Vector)
      return Import_Suggestions_Vector.Vector;
   --  For each declaration of Reachable_Declarations, determines a vector of
   --  valid with clauses and corresponding prefixes so that Identifier becomes
   --  visible.

end LAL_Refactor.Refactor_Imports;
