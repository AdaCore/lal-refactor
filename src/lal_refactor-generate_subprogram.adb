--
--  Copyright (C) 2025-2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--
with VSS.Strings; use VSS.Strings;
with VSS.Strings.Conversions;

with Laltools.Common;    use Laltools.Common;
with LAL_Refactor.Tools; use LAL_Refactor.Tools;
with LAL_Refactor.Stub_Utils;

package body LAL_Refactor.Generate_Subprogram is
   Tool_Name : constant String := "Generate Subprogram";
   --  When cursor is inside a subprogram declaration, look for a matching
   --  subprogram body. If no implementation is found,
   --  we can generate a subprogram body stub for the user to fill.

   function Start_Line (T : Token_Reference) return Line_Number
   is (T.Data.Sloc_Range.Start_Line);
   --  Helper to align node traversal with cursor position

   -------------------
   -- Get_Subp_Decl --
   -------------------

   function Get_Subp_Decl (Node : Ada_Node'Class) return Subp_Decl is
      Target_Subp : Subp_Decl := No_Subp_Decl;

      procedure Set_Subp_Decl (Parent : Ada_Node; Stop : in out Boolean);
      --  Write to Target_Subp if a parent Subp_Decl node is found

      procedure Set_Subp_Decl (Parent : Ada_Node; Stop : in out Boolean) is
      begin
         Stop := True;
         Target_Subp := Parent.As_Subp_Decl;
      end Set_Subp_Decl;
   begin
      if Is_Supported_Subp_Decl (Node) then
         Target_Subp := Node.As_Subp_Decl;
      else
         Find_Matching_Parents
           (Node, Is_Supported_Subp_Decl'Access, Set_Subp_Decl'Access);
      end if;
      return Target_Subp;
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

      function Is_Package_Decl (D : Basic_Decl'Class) return Boolean
      is (not D.P_Parent_Basic_Decl.Is_Null
          and then
            D.P_Parent_Basic_Decl.Kind
            in Ada_Base_Package_Decl | Ada_Generic_Package_Decl_Range);
      --  Public subprograms declared in a package must be placed
      --  into a separate file, which may not even exist
      --  TODO not currently supported

      function Subprogram_Decl_Has_Body (Sp_Decl : Subp_Decl) return Boolean
      is (not Sp_Decl.P_Body_Part.Is_Null);
      --  Don't generate a stub if the declaration already has a body

   begin
      Target_Subp := No_Subp_Decl;

      --  Check for a subprogram declaration on the same line
      if Node.Is_Null then
         return False;
      elsif Node.Kind in Ada_Ada_Node_List_Range | Ada_Declarative_Part_Range
      then
         --  If cursor is in whitespace before or after declaration,
         --  navigate forwards or backwards into non-trivial text
         if Start_Token.Is_Trivia then
            if Start_Line (Start_Token.Next (Exclude_Trivia => True))
              = Start_Loc.Line
            then
               Node := Lookup (Unit, Start_Token, Forward);
            elsif Start_Line (Start_Token.Previous (Exclude_Trivia => True))
              = Start_Loc.Line
            then
               Node := Lookup (Unit, Start_Token, Backward);
            end if;
         end if;
      end if;

      Target_Subp := Get_Subp_Decl (Node);
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

   ---------------------------------
   -- Create_Subprogram_Generator --
   ---------------------------------

   function Create_Subprogram_Generator
     (Target_Subp : Ada_Node'Class; Dest_Filename : String)
      return Subprogram_Generator
   is (Dest_Filename => To_Unbounded_String (Dest_Filename),
       Target_Subp   => Get_Subp_Decl (Target_Subp));

   --------------
   -- Refactor --
   --------------

   overriding
   function Refactor
     (Self           : Subprogram_Generator;
      Analysis_Units : access function return Analysis_Unit_Array)
      return Refactoring_Edits
   is

      function Generate (Subprogram : Subp_Decl) return Unbounded_String
      with Post => Generate'Result not in Null_Unbounded_String;
      --  Generate subprogram body text

      function Find_Local_Insertion_Point
        (Subprogram : Subp_Decl) return Source_Location;
      --  Calculate suitable text insertion point in declarative scope
      --  If a docstring is attached, insert beneath it

      --------------------------------
      -- Find_Local_Insertion_Point --
      --------------------------------

      function Find_Local_Insertion_Point
        (Subprogram : Subp_Decl) return Source_Location
      is
         End_Of_Scope    : constant Line_Number :=
           Subprogram.P_Declarative_Scope.Sloc_Range.End_Line;
         End_Of_Decl     : constant Line_Number :=
           Subprogram.Sloc_Range.End_Line;
         Line_After_Decl : constant Line_Number :=
           End_Of_Decl + Line_Number (1);
         Line_Cursor     : Line_Number := Line_After_Decl;
         Point           : Source_Location := (Line_Cursor, 1);
         AU              : constant Analysis_Unit := Subprogram.Unit;
         T               : Token_Reference := AU.Lookup_Token (Point);
      begin
         --  indicates empty line
         if T in No_Token then
            return Point;
         end if;

         while Line_Cursor < End_Of_Scope and T.Is_Trivia loop
            if T.Data.Kind in Ada_Comment then
               Line_Cursor := Line_Cursor + 1;
               Point.Line := Line_Cursor;
               T := AU.Lookup_Token (Point);
            else
               T := T.Next (Exclude_Trivia => False);
               exit when Start_Line (T) > Point.Line;
            end if;
         end loop;
         return Point;
      exception
         when E : others =>
            Refactor_Trace.Trace
              (E, "Could not identify context around subprogram declaration.");
            return (Line_After_Decl, 1);
      end Find_Local_Insertion_Point;

      --------------
      -- Generate --
      --------------

      function Generate (Subprogram : Subp_Decl) return Unbounded_String is
         use LAL_Refactor.Stub_Utils;
      begin
         return Create_Code_Generator (Subprogram).Generate_Body;
      end Generate;

      Insert_Point : constant Source_Location :=
        Find_Local_Insertion_Point (Self.Target_Subp);
      Insert_Range : constant Source_Location_Range :=
        Make_Range (Insert_Point, Insert_Point);
      --  Inserting new text without modifying or removing existing text
      --  only requires one insertion point location

      Text_Edits : Text_Edit_Ordered_Set;
      Edits      : Refactoring_Edits;
   begin
      Text_Edits.Insert ((Insert_Range, Generate (Self.Target_Subp)));
      Edits.Text_Edits.Insert (To_String (Self.Dest_Filename), Text_Edits);
      return Edits;
   exception
      when E : others =>
         Refactor_Trace.Trace
           (E, Refactoring_Tool_Refactor_Default_Error_Message (Tool_Name));
         return No_Refactoring_Edits;
   end Refactor;

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
