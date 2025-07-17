--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  This package contains refactoring tools that allow extracting statements
--  and expressions into new subprograms

package LAL_Refactor.Extract_Expression is

   function Is_Extract_Expression_Available
     (Unit               : Analysis_Unit;
      Section_To_Extract : in out Source_Location_Range)
      return Boolean;
   --  Checks if Section_To_Extract is an expression that can be extracted into
   --  a variable.

   function Default_Extracted_Expression_Name
     (Unit     : Analysis_Unit;
      Location : Source_Location)
      return Unbounded_String;
   --  Assuming the Location is within a set of expressions that can be
   --  extracted, returns a name for the extracted expression.
   --  The name will be "Extracted_<_N>" where N is a suffix to avoid
   --  collisions in the declarative part where the variable will be
   --  created.

   type Expression_Extractor is new Refactoring_Tool with private;

   function Create_Expression_Extractor
     (Unit               : Analysis_Unit;
      Section_To_Extract : Source_Location_Range;
      Variable_Name      : Unbounded_String)
      return Expression_Extractor;
   --  Expression_Extractor constructor.
   --  The section to extract needs to be validated by
   --  Is_Extract_Expression_Available.
   --  If the inputs are not valid, the Refactor might succeed but generate
   --  invalid code, or a Constrained_Error might be raised to to invalid
   --  node conversions.

   overriding
   function Refactor
     (Self           : Expression_Extractor;
      Analysis_Units : access function return Analysis_Unit_Array)
      return Refactoring_Edits;

private

   type Expression_Extractor is new Refactoring_Tool with
      record
         Unit : Analysis_Unit;
         Node : Ada_Node;
         Name : Unbounded_String;
      end record;

   package String_Vectors is new
     Ada.Containers.Indefinite_Vectors (Natural, String);

   subtype String_Vector is String_Vectors.Vector;

end LAL_Refactor.Extract_Expression;
