--
--  Copyright (C) 2021-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--   This package contains refactoring tools that allow suppressing a
--   separate subprogram

package LAL_Refactor.Suppress_Separate is

   function Is_Suppress_Separate_Available
     (Node            : Ada_Node;
      Target_Separate : out Basic_Decl)
      return Boolean;
   --  Checks if the Suppress Separate refactoring is available on Node.
   --  If so, sets 'Target_Separate' with the appropriate values to be used by
   --  the function 'Suppress_Separate' or to create a 'Separate_Suppressor'
   --  object
   --  Otherwise, sets 'Target_Separate' to 'No_Subunit';

   function Suppress_Separate
     (Target_Separate : Basic_Decl)
      return Refactoring_Edits
     with Pre => not Target_Separate.Is_Null;
   --  Returns all the needed edits to suppress a 'Subunit'.
   --  'Refactoring_Edits' will contain the file that needs to be deleted and
   --  the text edits needed in the separate's parent package body.

   type Separate_Suppressor is new Refactoring_Tool with private;

   overriding
   function Refactor
     (Self           : Separate_Suppressor;
      Analysis_Units : access function return Analysis_Unit_Array := null)
      return Refactoring_Edits;
   --  Runs the refactoring analysis and returns all the needed edits to
   --  suppress a 'Subunit'. The 'Refactoring_Edits' will contain the file that
   --  needs to be deleted and the text edits needed in the separate's parent
   --  package body.
   --  'Analysis_Units' parameter is not nedded for this 'Refactoring_Tool' and
   --  is therefore unused.

   function Create
     (Target_Separate : Basic_Decl)
      return Separate_Suppressor
     with Pre => not Target_Separate.Is_Null;
   --  Creates a Separate_Suppressor that will suppress 'Target_Separate'

private

   type Separate_Suppressor is new Refactoring_Tool with
      record
         Target_Separate : Basic_Decl;
      end record;

end LAL_Refactor.Suppress_Separate;
