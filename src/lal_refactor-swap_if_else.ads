--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  This package contains refactoring tools that swaps if/elsif/else conditions
--   and code statments

package LAL_Refactor.Swap_If_Else is

   function Is_Swap_Available
     (Unit     : Analysis_Unit;
      Location : Source_Location)
      return Boolean;
   --  Checks if Location is inside an `if not` statment that also have `else`.

   type Swaper is new Refactoring_Tool with private;

   function Create_Swaper
     (Unit     : Analysis_Unit;
      Location : Source_Location)
      return Swaper;
   --  Swaper constructor.
   --  The section needs to be validated by Is_Swap_Available.
   --  If the inputs are not valid, the Refactor might succeed but generate
   --  invalid code, or a Constrained_Error might be raised to to invalid
   --  node conversions.

   overriding
   function Refactor
     (Self           : Swaper;
      Analysis_Units : access function return Analysis_Unit_Array)
      return Refactoring_Edits;

private

   type Swaper is new Refactoring_Tool with
      record
         Unit     : Analysis_Unit;
         Location : Source_Location;
      end record;

end LAL_Refactor.Swap_If_Else;
