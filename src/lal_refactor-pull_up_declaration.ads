--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  This package contains refactoring tools that allow pulling up
--  declarations to an upper level declarative part.
--  Currently, there is one limitation: declarations of nested packages
--  cannot be pulled up.

package LAL_Refactor.Pull_Up_Declaration is

   Invalid_Declaration : exception;

   function Is_Pull_Up_Declaration_Available
     (Unit      : Analysis_Unit;
      Node_SLOC : Source_Location)
      return Boolean;
   --  Checks if Unit and Declaration_SLOC represent a declaration that can be
   --  extracted. If so, Unit and Declaration_SLOC can be used on the
   --  Declaration_Extractor constructor Create_Declaration_Extractor.

   type Declaration_Extractor is new Refactoring_Tool with private;

   function Create_Declaration_Pull_Upper
     (Unit                           : Analysis_Unit;
      Definition_SLOC                : Source_Location;
      Indentation                    : Natural := 3;
      Only_Dependencies              : Boolean := False;
      Try_Subp_Body_Insertion_Point  : Boolean := False;
      Use_Parent_Decl_Canonical_Part : Boolean := False)
      return Declaration_Extractor
     with Pre => (Unit /= No_Analysis_Unit
                  and Definition_SLOC /= No_Source_Location)
                 and then Is_Pull_Up_Declaration_Available
                            (Unit, Definition_SLOC);
   --  Declaration_Extractor constructor.
   --  Declaration_SLOC must be the SLOC of the declaration Name'Class node
   --  that will be extracted. Use Is_Pull_Up_Declaration_Available to check
   --  if a declaration can be extracted from Unit and Declaration_SLOC.
   --  Raises an Invalid_Declaration if it fails to resolve the declaration
   --  based on Unit and Definition_SLOC.

   overriding
   function Refactor
     (Self           : Declaration_Extractor;
      Analysis_Units : access function return Analysis_Unit_Array)
      return Refactoring_Edits;
   --  Extracts the declaration identified by the Unit and Declaration_SLOC
   --  passed to the Declaration_Extractor constructor
   --  Create_Declaration_Extractor.

private

   type Declaration_Extractor is new Refactoring_Tool with
      record
         Definition                     : Defining_Name;
         Indentation                    : Natural := 3;
         Only_Dependencies              : Boolean := False;
         Try_Subp_Body_Insertion_Point  : Boolean := False;
         Use_Parent_Decl_Canonical_Part : Boolean := False;
      end record
     with Dynamic_Predicate => not Definition.Is_Null;

end LAL_Refactor.Pull_Up_Declaration;
