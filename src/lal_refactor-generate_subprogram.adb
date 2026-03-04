--
--  Copyright (C) 2025-2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with VSS.Strings;        use VSS.Strings;
with VSS.Strings.Conversions;

with LAL_Refactor.Utils;
with LAL_Refactor.Tools; use LAL_Refactor.Tools;
with LAL_Refactor.Stub_Utils;
with LAL_Refactor.Generate_Package;

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
      N : Ada_Node := Node.As_Ada_Node;
   begin
      while not N.Is_Null loop
         exit when Valid_Subp_Decl (N);
         N := N.P_Parent_Basic_Decl.As_Ada_Node;
      end loop;
      return (if N.Is_Null then No_Subp_Decl else N.As_Subp_Decl);
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

      function Is_Unimplemented (Sp_Decl : Subp_Decl) return Boolean
      is (Sp_Decl.P_Body_Part.Is_Null);
      --  Check local and package scopes for an existing implementation

   begin
      Target_Subp := No_Subp_Decl;

      --  Check for a subprogram declaration on the same line
      if Node.Is_Null then
         return False;
      elsif Node.Kind in Ada_Ada_Node_List_Range | Ada_Declarative_Part_Range
        and Start_Token.Is_Trivia
      then
         --  If cursor is in whitespace or comment,
         --  scan nearest non-trivial text on the same line
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

      Target_Subp := Get_Subp_Decl (Node);
      return not Target_Subp.Is_Null and then Is_Unimplemented (Target_Subp);
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
     (Node : Ada_Node'Class) return Subprogram_Generator
   is
      Subprogram   : constant Subp_Decl := Get_Subp_Decl (Node);
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
      is (Create_Code_Generator (Subprogram).Generate_Body)
      with Post => Generate'Result not in Null_Unbounded_String;
      --  Generate subprogram body text

      Edits     : Refactoring_Edits;
      Dest_File : constant String :=
        (if Self.Action in Local
         then Self.Target_Subp.Unit.Get_Filename
         else
           LAL_Refactor.Generate_Package.Get_Body_Path
             (Get_Parent_Package_Spec (Self.Target_Subp)));
   begin
      case Self.Action is
         when New_Pkg_Body            =>
            declare
               File_Path              : constant Unbounded_String :=
                 To_Unbounded_String (Dest_File);
               Spec                   : constant Base_Package_Decl :=
                 Get_Parent_Package_Spec (Self.Target_Subp);
               Set                    : constant Subp_Set :=
                 To_Subp_Set (Self.Target_Subp);
               Generated_Package_Text : constant Unbounded_String :=
                 Create_Code_Generator (Spec => Spec, Subprograms => Set)
                   .Generate_Body;
            begin
               Edits.File_Creations.Insert
                 ((File_Path, Generated_Package_Text));
            end;

         when Local | Add_To_Pkg_Body =>
            declare
               Generated_Stub  : constant Unbounded_String :=
                 Generate (Self.Target_Subp);
               Insert_Point    : constant Source_Location :=
                 LAL_Refactor.Utils.Get_Contextual_Insertion_Point
                   (Self.Target_Subp);
               Insert_Location : constant Source_Location_Range :=
                 Make_Range (Insert_Point, Insert_Point);
               Stub_Edit       : constant Text_Edit :=
                 (Location => Insert_Location, Text => Generated_Stub);
            begin
               Safe_Insert (Edits.Text_Edits, Dest_File, Stub_Edit);
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
