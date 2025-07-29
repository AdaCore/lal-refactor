--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  This package contains refactoring tools that allow sort `where` in
--  case statments

package LAL_Refactor.Sort_Case is

   function Is_Sort_Alphabetically_Available
     (Unit     : Analysis_Unit;
      Location : in out Source_Location)
      return Boolean;
   --  Checks if Location is in a case statment than can be
   --  sort alphabetically.

   function Is_Sort_Declaration_Available
     (Unit     : Analysis_Unit;
      Location : in out Source_Location)
      return Boolean;
   --  Checks if Location is in a case statment than can be
   --  sort according to the declaration.

   type Alphabetical_Case_Sorter is new Refactoring_Tool with private;

   function Create_Alphabetical_Case_Sorter
     (Unit     : Analysis_Unit;
      Location : Source_Location)
      return Alphabetical_Case_Sorter;
   --  Alphabetical_Case_Sorter constructor.
   --  The Location needs to be validated by Is_Sort_Alphabetically_Available.
   --  If the inputs are not valid, the Refactor might succeed but generate
   --  invalid code, or a Constrained_Error might be raised to to invalid
   --  node conversions.

   overriding
   function Refactor
     (Self           : Alphabetical_Case_Sorter;
      Analysis_Units : access function return Analysis_Unit_Array)
      return Refactoring_Edits;

   type Declaration_Case_Sorter is new Refactoring_Tool with private;

   function Create_Declaration_Case_Sorter
     (Unit     : Analysis_Unit;
      Location : Source_Location)
      return Declaration_Case_Sorter;
   --  Declaration_Case_Sorter constructor.
   --  The Location needs to be validated by Is_Sort_Declaration_Available.
   --  If the inputs are not valid, the Refactor might succeed but generate
   --  invalid code, or a Constrained_Error might be raised to to invalid
   --  node conversions.

   overriding
   function Refactor
     (Self           : Declaration_Case_Sorter;
      Analysis_Units : access function return Analysis_Unit_Array)
      return Refactoring_Edits;

private

   type Alphabetical_Case_Sorter is new Refactoring_Tool with
      record
         Unit : Analysis_Unit;
         Node : Ada_Node;
      end record;

   type Declaration_Case_Sorter is new Refactoring_Tool with
      record
         Unit : Analysis_Unit;
         Node : Ada_Node;
      end record;

end LAL_Refactor.Sort_Case;
