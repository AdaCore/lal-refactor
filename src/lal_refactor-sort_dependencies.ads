--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  This package contains refactoring tools that allow sorting a compilation
--  unit prelude in alphabetical order.

package LAL_Refactor.Sort_Dependencies is

   function Is_Sort_Dependencies_Available
     (Unit             : Analysis_Unit;
      Sloc             : Source_Location)
      return Boolean;
   --  Returns True if Sloc is inside a compilation unit prelude

   type Dependencies_Sorter is new Refactoring_Tool with private;

   function Create_Dependencies_Sorter
     (Compilation_Unit : Libadalang.Analysis.Compilation_Unit;
      No_Separator     : Boolean := False)
     return Dependencies_Sorter;
   --  Dependencies_Sorter constructor
   --  Compilation_Unit is the unit that Dependencies_Sorter will use to
   --  format the prelude.
   --  If No_Separator is True, Dependencies_Sorter will not separate with/use
   --  clauses groups by a new line (in this context, a group is composed
   --  by with/use clauses that have the same parent package).

   overriding
   function Refactor
     (Self           : Dependencies_Sorter;
      Analysis_Units : access function return Analysis_Unit_Array)
      return Refactoring_Edits;
   --  Sorts the Compilation_Unit's preludes passed to Dependencies_Sorter's
   --  constructor Create_Dependencies_Sorter.
   --  The prelude is sorted alphabetically.

private

   type Dependencies_Sorter is new Refactoring_Tool with
      record
         Compilation_Unit : Libadalang.Analysis.Compilation_Unit;
         No_Separator     : Boolean;
         --  If True, do not add an empty line between with/use clauses groups
      end record;

end LAL_Refactor.Sort_Dependencies;
