--
--  Copyright (C) 2025-2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--
with VSS.Strings; use VSS.Strings;
with VSS.Strings.Conversions;

with Libadalang.Common;  use Libadalang.Common;
with Laltools.Common;    use Laltools.Common;
with LAL_Refactor.Tools; use LAL_Refactor.Tools;
with LAL_Refactor.Stub_Utils;

package body LAL_Refactor.Generate_Subprogram is
   Tool_Name : constant String := "Generate Subprogram";
   --  When cursor is inside a subprogram declaration, look for a matching
   --  subprogram body. If no implementation is found,
   --  we can generate a subprogram body stub for the user to fill.

   -------------------
   -- Get_Subp_Decl --
   -------------------

   function Get_Subp_Decl (Node : Ada_Node'Class) return Subp_Decl is
   begin
      if Node.Is_Null then
         return No_Subp_Decl;
      elsif Node.Kind in Ada_Subp_Decl_Range then
         return Node.As_Subp_Decl;
      elsif not Node.P_Parent_Basic_Decl.Is_Null
        and then Node.P_Parent_Basic_Decl.Kind in Ada_Subp_Decl_Range
      then
         return Node.P_Parent_Basic_Decl.As_Subp_Decl;
      else
         return No_Subp_Decl;
      end if;
   end Get_Subp_Decl;

   --------------------------------------
   -- Is_Generate_Subprogram_Available --
   --------------------------------------

   function Is_Generate_Subprogram_Available
     (Unit        : Analysis_Unit;
      Start_Loc   : Source_Location;
      Target_Subp : out Subp_Decl) return Boolean
   is
      Start_Token : constant Token_Reference := Unit.Lookup_Token (Start_Loc);
      Node        : Ada_Node := Unit.Root.Lookup (Start_Loc);

      function Start_Line (T : Token_Reference) return Line_Number
      is (Sloc_Range (Data (T)).Start_Line);
      --  Helper to check node matches cursor line

      procedure Set_Subp_Decl (Parent : Ada_Node; Stop : in out Boolean);
      --  Write to Target_Subp if a parent Subp_Decl node is found

      function Is_Subp_Decl (Node : Ada_Node'Class) return Boolean
      is (Node.Kind in Ada_Subp_Decl_Range);

      function Is_Package_Decl (D : Basic_Decl'Class) return Boolean
      is (not D.P_Parent_Basic_Decl.Is_Null
          and then
            D.P_Parent_Basic_Decl.Kind
            in Ada_Base_Package_Decl | Ada_Generic_Package_Decl_Range);
      --  Check if declaration is a top-level decl from a package spec
      --  in which case a body must be generated in a different file

      function Subprogram_Decl_Has_Body (Sp_Decl : Subp_Decl) return Boolean
      is (not Sp_Decl.P_Body_Part.Is_Null);
      --  Check for a matching subprogram body

      procedure Set_Subp_Decl (Parent : Ada_Node; Stop : in out Boolean) is
      begin
         Stop := True;
         Target_Subp := Parent.As_Subp_Decl;
      end Set_Subp_Decl;
   begin
      Target_Subp := No_Subp_Decl;

      --  Check for a subprogram declaration on the same line
      if Node.Is_Null then
         return False;
      elsif Node.Kind in Ada_Ada_Node_List_Range | Ada_Declarative_Part_Range
      then
         --  If Start_Loc begins in whitespace between declarations,
         --  then Node will point to the parent declaration list instead
         --  of its child declarations. Navigate out of whitespace to
         --  search for a declaration on the same line.

         if Kind (Data (Start_Token)) = Ada_Whitespace then
            if Start_Line (Next_Non_Whitespace (Start_Token, Forward))
              = Start_Loc.Line
            then
               Node := Lookup (Unit, Start_Token, Forward);
            elsif Start_Line (Next_Non_Whitespace (Start_Token, Backward))
              = Start_Loc.Line
            then
               Node := Lookup (Unit, Start_Token, Backward);
            end if;
         end if;
      end if;

      --  Now check if Node is inside a subprogram declaration
      if Is_Subp_Decl (Node) then
         Target_Subp := Node.As_Subp_Decl;
      else
         Find_Matching_Parents
           (Node, Is_Subp_Decl'Access, Set_Subp_Decl'Access);
         --  This ensures that if we are inside a child node such as a
         --  parameter declaration or return type,
         --  we still navigate to the parent subprogram declaration
      end if;
      return
        not Target_Subp.Is_Null
        and then
          not (Is_Package_Decl (Target_Subp)
               or Subprogram_Decl_Has_Body (Target_Subp));
   exception
      when E : others =>
         Refactor_Trace.Trace
           (E,
            Is_Refactoring_Tool_Available_Default_Error_Message (Tool_Name));
         return False;
   end Is_Generate_Subprogram_Available;

   --------------
   -- Refactor --
   --------------

   overriding
   function Refactor
     (Self           : Subprogram_Generator;
      Analysis_Units : access function return Analysis_Unit_Array)
      return Refactoring_Edits is
   begin
      return Self.Generate;
   exception
      when E : others =>
         Refactor_Trace.Trace
           (E, Refactoring_Tool_Refactor_Default_Error_Message (Tool_Name));
         return No_Refactoring_Edits;
   end Refactor;

   ---------------------------------
   -- Create_Subprogram_Generator --
   ---------------------------------

   function Create_Subprogram_Generator
     (Target_Subp : Ada_Node'Class; Dest_Filename : String)
      return Subprogram_Generator is
   begin
      return
        (Dest_Filename => To_Unbounded_String (Dest_Filename),
         Target_Subp   => Get_Subp_Decl (Target_Subp));
      --  Constraint_Error if Get_Subp_Decl fails
   end Create_Subprogram_Generator;

   -------------------------
   -- Get_Insertion_Point --
   -------------------------

   function Get_Insertion_Point
     (Self : Subprogram_Generator) return Source_Location_Range is
   begin
      return
        (Start_Line   | End_Line   => Self.Target_Subp.Sloc_Range.End_Line + 1,
         Start_Column | End_Column => 1);
   end Get_Insertion_Point;

   --------------
   -- Generate --
   --------------

   function Generate (Self : Subprogram_Generator) return Refactoring_Edits is
      use LAL_Refactor.Stub_Utils;

      Generated_Subprogram : constant Unbounded_String :=
        Create_Code_Generator (Self.Target_Subp).Generate_Subprogram_Body;
      Insertion_Point      : constant Source_Location_Range :=
        Self.Get_Insertion_Point;
      Text_Edits           : Text_Edit_Ordered_Set;
      Edits                : Refactoring_Edits;
   begin
      Text_Edits.Insert ((Insertion_Point, Generated_Subprogram));
      Edits.Text_Edits.Insert (To_String (Self.Dest_Filename), Text_Edits);

      return Edits;
   end Generate;

   -- LSP diagnostics --

   --------------
   -- Filename --
   --------------

   overriding
   function Filename (Self : Generate_Subprogram_Problem) return String
   is (Conversions.To_UTF_8_String (Self.Filename));

   --------------
   -- Location --
   --------------

   overriding
   function Location
     (Self : Generate_Subprogram_Problem) return Source_Location_Range
   is (Self.Location);

   ----------
   -- Info --
   ----------

   overriding
   function Info (Self : Generate_Subprogram_Problem) return String
   is (Conversions.To_UTF_8_String (Self.Info));

   ------------------
   -- Report_Error --
   ------------------

   function Report_Error
     (Msg, File : String; SLOC : Source_Location_Range)
      return Generate_Subprogram_Problem
   is (Info     => Conversions.To_Virtual_String (Msg),
       Location => SLOC,
       Filename => Conversions.To_Virtual_String (File));
   --  Use this if Node.Is_Null

   ------------------
   -- Report_Error --
   ------------------

   function Report_Error
     (Msg : String; Node : Ada_Node'Class) return Generate_Subprogram_Problem
   is (Info     => Conversions.To_Virtual_String (Msg),
       Location =>
         (if Node.Is_Null then No_Source_Location_Range else Node.Sloc_Range),
       Filename =>
         (if Node.Is_Null or else Node.Unit in No_Analysis_Unit
          then Empty_Virtual_String
          else Conversions.To_Virtual_String (Node.Unit.Get_Filename)));
   --  Do not use this if Node.Is_Null, it will not display an error at all

end LAL_Refactor.Generate_Subprogram;
