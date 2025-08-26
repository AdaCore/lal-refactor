--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  This package contains LAL_Tools utilities to be used by delete entity
--  refactoring.

package LAL_Refactor.Delete_Entity is

   Invalid_Declaration : exception;

   function Is_Delete_Entity_Available
     (Unit      : Analysis_Unit;
      Node_SLOC : Source_Location)
      return Boolean;
   --  Checks if Unit and Node_SLOC represent a declaration that can be
   --  deleted. If so, Unit and Node_SLOC can be used on the
   --  Entity_Remover constructor Create_Entity_Remover.

   type Entity_Remover is new Refactoring_Tool with private;

   function Create_Entity_Deleter
     (Unit            : Analysis_Unit;
      Definition_SLOC : Source_Location)
      return Entity_Remover
     with Pre => Is_Delete_Entity_Available (Unit, Definition_SLOC);
   --  Entity_Remover constructor.
   --  Definition_SLOC must be the SLOC of the declaration Name'Class node
   --  that will be deleted. Use Is_Delete_Entity_Available to check if a
   --  declaration can be deleted from Unit and Declaration_SLOC.
   --  Raises an Invalid_Declaration if it fails to resolve the declaration
   --  based on Unit and Definition_SLOC.

   overriding
   function Refactor
     (Self           : Entity_Remover;
      Analysis_Units : access function return Analysis_Unit_Array)
      return Refactoring_Edits;
   --  Deletes the declaration identified by the Unit and Declaration_SLOC
   --  passed to the Entity_Remover constructor Create_Entity_Remover.

private

   type Entity_Remover is new Refactoring_Tool with
      record
         Definition : Defining_Name;
      end record
     with Dynamic_Predicate => not Definition.Is_Null;

end LAL_Refactor.Delete_Entity;
