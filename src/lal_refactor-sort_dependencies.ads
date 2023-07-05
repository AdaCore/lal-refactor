--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  This package contains refactoring tools that allow sorting a compilation
--  unit prelude.

package LAL_Refactor.Sort_Dependencies is

   function Is_Sort_Dependencies_Available
     (Unit             : Analysis_Unit;
      Sloc             : Source_Location)
      return Boolean;
   --  Returns True if Sloc is inside a compilation unit prelude

   type Dependencies_Sorter is new Refactoring_Tool with private;

   function Create_Dependencies_Sorter
     (Compilation_Unit : Libadalang.Analysis.Compilation_Unit)
     return Dependencies_Sorter;
   --  Dependencies_Sorter constructor

   overriding
   function Refactor
     (Self           : Dependencies_Sorter;
      Analysis_Units : access function return Analysis_Unit_Array)
      return Refactoring_Edits;
   --  Sorts the Compilation_Unit's preludes passed to Dependencies_Sorter's
   --  constructor Create_Dependencies_Sorter.

private

   type Dependencies_Sorter is new Refactoring_Tool with
      record
         Compilation_Unit : Libadalang.Analysis.Compilation_Unit;
      end record;

end LAL_Refactor.Sort_Dependencies;
