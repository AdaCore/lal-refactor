--
--  Copyright (C) 2025-2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--
with LAL_Refactor.Generate_Package;
with LAL_Refactor.Utils;
with VSS.Strings; use VSS.Strings;
with VSS.Strings.Conversions;

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
      Decl        : Basic_Decl;
   begin
      if Valid_Subp_Decl (Node) then
         Target_Subp := Node.As_Subp_Decl;
      else
         Decl := Node.P_Parent_Basic_Decl;
         while not Decl.Is_Null loop
            exit when Valid_Subp_Decl (Decl);
            Decl := Decl.P_Parent_Basic_Decl;
         end loop;
         if not Decl.Is_Null then
            Target_Subp := Decl.As_Subp_Decl;
         end if;
      end if;
      return Target_Subp;
   end Get_Subp_Decl;

   -----------------------------
   -- Get_Parent_Package_Spec --
   -----------------------------

   function Get_Parent_Package_Spec (D : Subp_Decl) return Base_Package_Decl is
   begin
      if not D.P_Semantic_Parent.Is_Null
        and then D.P_Semantic_Parent.Kind in Ada_Base_Package_Decl
      then
         return D.P_Semantic_Parent.As_Base_Package_Decl;
      elsif not D.P_Parent_Basic_Decl.Is_Null
        and then D.P_Parent_Basic_Decl.Kind in Ada_Base_Package_Decl
      then
         return D.P_Parent_Basic_Decl.As_Base_Package_Decl;
      else
         return No_Base_Package_Decl;
      end if;
   end Get_Parent_Package_Spec;

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

      function Subprogram_Unimplemented (Sp_Decl : Subp_Decl) return Boolean
      is (Sp_Decl.P_Body_Part.Is_Null);
      --  Check local and package scopes for an existing implementation

   begin
      Target_Subp := No_Subp_Decl;

      --  Check for a subprogram declaration on the same line
      if Node.Is_Null then
         return False;
      elsif Node.Kind in Ada_Ada_Node_List_Range | Ada_Declarative_Part_Range
      then
         --  If cursor is in whitespace or comment,
         --  scan nearest non-trivial text on the same line
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
        and then Subprogram_Unimplemented (Target_Subp);
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
     (Target_Subp : Ada_Node'Class) return Subprogram_Generator
   is
      Subprogram   : constant Subp_Decl := Get_Subp_Decl (Target_Subp);
      Package_Spec : constant Base_Package_Decl :=
        Get_Parent_Package_Spec (Subprogram);
      Insert_Mode  : constant Generate_Mode :=
        (if Package_Spec.Is_Null
         then Local
         else
           (if Package_Spec.P_Body_Part.Is_Null
            then New_Pkg_Body
            else Add_To_Pkg_Body));
   begin
      return (Target_Subp => Subprogram, Action => Insert_Mode);
   end Create_Subprogram_Generator;

   --------------
   -- Refactor --
   --------------

   overriding
   function Refactor
     (Self           : Subprogram_Generator;
      Analysis_Units : access function return Analysis_Unit_Array)
      return Refactoring_Edits
   is
      use LAL_Refactor.Stub_Utils;

      function Generate (Subprogram : Subp_Decl) return Unbounded_String
      with Post => Generate'Result not in Null_Unbounded_String;
      --  Generate subprogram body text

      function Find_Local_Insertion_Point
        (Subprogram : Subp_Decl) return Source_Location
      with Post => Find_Local_Insertion_Point'Result not in No_Source_Location;
      --  Calculate suitable text insertion point in declarative scope
      --  If a docstring is attached, insert beneath it

      --------------------------------
      -- Find_Local_Insertion_Point --
      --------------------------------

      function Find_Local_Insertion_Point
        (Subprogram : Subp_Decl) return Source_Location
      is
         Full_Node_Sloc : constant Source_Location_Range :=
           LAL_Refactor.Utils.Expand_SLOC_To_Docstring (Subprogram);
      begin
         return (Full_Node_Sloc.End_Line + Line_Number (1), Column_Number (1));
      end Find_Local_Insertion_Point;

      --------------
      -- Generate --
      --------------

      function Generate (Subprogram : Subp_Decl) return Unbounded_String is
      begin
         return Create_Code_Generator (Subprogram).Generate_Body;
      end Generate;

      Edits : Refactoring_Edits;

   begin
      case Self.Action is
         when Local                          =>
            declare
               Insert_Point   : constant Source_Location :=
                 Find_Local_Insertion_Point (Self.Target_Subp);
               Insert_Range   : constant Source_Location_Range :=
                 Make_Range (Insert_Point, Insert_Point);
               Generated_Stub : constant Unbounded_String :=
                 Generate (Self.Target_Subp);
               Destination    : constant String :=
                 Self.Target_Subp.Unit.Get_Filename;
               Subp_Stub      : constant Text_Edit :=
                 (Location => Insert_Range, Text => Generated_Stub);
            begin
               Safe_Insert (Edits.Text_Edits, Destination, Subp_Stub);
            end;

         when Add_To_Pkg_Body | New_Pkg_Body =>
            declare
               use LAL_Refactor.Generate_Package;

               Spec        : constant Base_Package_Decl :=
                 Get_Parent_Package_Spec (Self.Target_Subp);
               Pkg_Body    : constant Package_Body := Spec.P_Body_Part;
               Decls       : constant LAL_Refactor.Stub_Utils.Decl_Vector :=
                 Declaration_Vectors.To_Vector (Self.Target_Subp, 1);
               Destination : constant Unbounded_String :=
                 To_Unbounded_String (Get_Body_Path (Spec));
            begin
               if Pkg_Body.Is_Null then
                  Add_New_Package_Edits
                    (Edits      => Edits,
                     From_Spec  => Spec,
                     With_Decls => Decls,
                     To_Path    => Destination);
               else
                  Update_Package_Edits
                    (Edits     => Edits,
                     New_Decls => Decls,
                     To_Body   => Pkg_Body);
               end if;
            end;
      end case;

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
