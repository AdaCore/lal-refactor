--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  This package contains the Change Parameters Type refactoring tool that
--  allow changing the type of one or more parameters of a subprogram.

package LAL_Refactor.Subprogram_Signature.Change_Parameters_Type is

   function Is_Change_Parameters_Type_Available
     (Unit                             : Analysis_Unit;
      Parameters_Source_Location_Range : Source_Location_Range;
      New_Parameter_Syntax_Rules       : out Grammar_Rule_Vector)
      return Boolean;
   --  Checks if Parameters_Source_Location_Range correspondes to a selection
   --  of one or more parameters of the same Param_Spec. If so, returns True
   --  and sets New_Parameter_Syntax_Rules to the allowed syntax rules for the
   --  new parameter. Otherwise returns False and sets
   --  New_Parameter_Syntax_Rules to an empty vector.

   type Parameters_Type_Changer is new
     Subprogram_Signature_Changer with private;

   function Create_Parameters_Type_Changer
     (Unit                             : Analysis_Unit;
      Parameters_Source_Location_Range : Source_Location_Range;
      New_Parameters_Type              : Unbounded_String;
      Configuration                    :
        Signature_Changer_Configuration_Type := Default_Configuration)
      return Parameters_Type_Changer;
   --  Parameters_Type_Changer constructor.
   --  Creates a subprogram signature changer that changes parameters type.
   --  Parameters_Source_Location_Range must be the Source_Location_Range
   --  of the parameters, where Start_Sloc (Parameters_Source_Location_Range)
   --  is the first parameter Name node, and
   --  End_Sloc (Parameters_Source_Location_Range) is the last one. If they're
   --  the same, then only one parameter is modified.
   --  Configuration controls what subprograms in the hierarchy are affected.
   --  Unit and Parameters_Source_Location_Range should be validated by
   --  Is_Change_Parameters_Type_Available.

   overriding
   function Refactor
     (Self           : Parameters_Type_Changer;
      Analysis_Units : access function return Analysis_Unit_Array)
      return Refactoring_Edits;
   --  Returns an Edit_Map with all the refactoring edits needed to change
   --  the parameters type.

private

   type Parameters_Type_Changer is new
     Subprogram_Signature_Changer with
      record
         Unit                             : Analysis_Unit;
         Parameters_Source_Location_Range : Source_Location_Range;
         New_Parameters_Type              : Unbounded_String;
         Configuration                    :
           Signature_Changer_Configuration_Type := Default_Configuration;
      end record;

end LAL_Refactor.Subprogram_Signature.Change_Parameters_Type;
