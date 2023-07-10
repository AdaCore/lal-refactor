--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Libadalang.Common; use Libadalang.Common;

package body LAL_Refactor.Replace_Type is

   -------------------------------
   -- Is_Replace_Type_Available --
   -------------------------------

   function Is_Replace_Type_Available
     (Source_Unit     : Analysis_Unit;
      Source_Location : Langkit_Support.Slocs.Source_Location)
      return Boolean
   is
      Source_Node : constant Ada_Node :=
        (if Source_Unit = No_Analysis_Unit
           or else Source_Location = No_Source_Location
         then
            No_Ada_Node
         else
            Source_Unit.Root.Lookup (Source_Location));

   begin
      if not Source_Node.Is_Null
        and then Source_Node.Kind in Ada_Name
      then
         declare
            Source_Node_Parent_Decl : constant Basic_Decl :=
              Source_Node.P_Parent_Basic_Decl;

         begin
            return not Source_Node_Parent_Decl.Is_Null
              and then Source_Node_Parent_Decl.Kind in Ada_Base_Type_Decl;
         end;
      else
         return False;
      end if;
   end Is_Replace_Type_Available;

   --------------------------
   -- Create_Type_Replacer --
   --------------------------

   function Create_Type_Replacer
     (Source_Unit      : Analysis_Unit;
      Source_Type_SLOC : Source_Location;
      New_Type         : Unbounded_String)
      return Type_Replacer
   is
      Node : constant Ada_Node := Source_Unit.Root.Lookup (Source_Type_SLOC);

   begin
      return Type_Replacer'
               (Node.P_Parent_Basic_Decl.P_Canonical_Part.As_Base_Type_Decl,
                New_Type);
   end Create_Type_Replacer;

   --------------
   -- Refactor --
   --------------

   overriding
   function Refactor
     (Self           : Type_Replacer;
      Analysis_Units : access function return Analysis_Unit_Array)
      return Refactoring_Edits
   is
      Units : constant Analysis_Unit_Array := Analysis_Units.all;

      Text_Edits : Text_Edit_Map;

   begin
      for Reference of
        Self.Source_Type.P_Defining_Name.P_Find_All_References (Units)
      loop
         declare
            Reference_Base_Id             : constant Base_Id'Class :=
              Ref (Reference);
            Reference_Base_Id_Parent_Decl : constant Basic_Decl :=
              (declare Parent_Decl : constant Basic_Decl :=
                 Reference_Base_Id.P_Parent_Basic_Decl;
               begin
                 (if not Parent_Decl.Is_Null then Parent_Decl.P_Canonical_Part
                  else No_Basic_Decl));
         begin
            if Self.Source_Type /= Reference_Base_Id_Parent_Decl then
               Safe_Insert
                 (Edits     => Text_Edits,
                  File_Name => Reference_Base_Id.Unit.Get_Filename,
                  Edit      => Text_Edit'
                                 (Location =>
                                    (if Reference_Base_Id.Parent.Kind in
                                       Ada_Dotted_Name
                                     then
                                        Reference_Base_Id.Parent.Sloc_Range
                                     else
                                        Reference_Base_Id.Sloc_Range),
                                  Text     => Self.New_Type));
            end if;
         end;
      end loop;

      return Refactoring_Edits'(Text_Edits => Text_Edits, others => <>);
   end Refactor;

end LAL_Refactor.Replace_Type;
