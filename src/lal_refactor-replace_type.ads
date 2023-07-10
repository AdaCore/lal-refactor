--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Langkit_Support.Slocs;

package LAL_Refactor.Replace_Type is

   function Is_Replace_Type_Available
     (Source_Unit     : Analysis_Unit;
      Source_Location : Langkit_Support.Slocs.Source_Location)
      return Boolean;
   --  TODO

   type Type_Replacer is new Refactoring_Tool with private;

   function Create_Type_Replacer
     (Source_Unit      : Analysis_Unit;
      Source_Type_SLOC : Langkit_Support.Slocs.Source_Location;
      New_Type         : Unbounded_String)
      return Type_Replacer;
   --  TODO

   overriding
   function Refactor
     (Self           : Type_Replacer;
      Analysis_Units : access function return Analysis_Unit_Array)
      return Refactoring_Edits;
   --  TODO

private

   type Type_Replacer is new Refactoring_Tool with
      record
         Source_Type : Base_Type_Decl;
         New_Type    : Unbounded_String;
      end record;

end LAL_Refactor.Replace_Type;
