--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  This package contains refactoring tools that allow extracting expressions
--  into new variables

package LAL_Refactor.Inline_Variable is

   function Is_Inline_Variable_Available
     (Unit           : Analysis_Unit;
      Location       : Source_Location;
      Analysis_Units : access function return Analysis_Unit_Array)
      return Boolean;
   --  Checks if Location is an variable declaration with the initialization
   --  expression that can be set instead of the variable.

   type Variable_Inliner is new Refactoring_Tool with private;

   function Create_Variable_Inliner
     (Unit     : Analysis_Unit;
      Location : Source_Location)
      return Variable_Inliner;
   --  Variable_Inliner constructor.
   --  The section to extract needs to be validated by
   --  Is_Inline_Variable_Available.
   --  If the inputs are not valid, the Refactor might succeed but generate
   --  invalid code, or a Constrained_Error might be raised to to invalid
   --  node conversions.

   overriding
   function Refactor
     (Self           : Variable_Inliner;
      Analysis_Units : access function return Analysis_Unit_Array)
      return Refactoring_Edits;

private

   type Variable_Inliner is new Refactoring_Tool with
      record
         Unit     : Analysis_Unit;
         Node     : Ada_Node;
         Location : Source_Location;
      end record;

end LAL_Refactor.Inline_Variable;
