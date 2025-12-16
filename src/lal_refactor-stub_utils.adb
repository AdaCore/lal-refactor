--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Libadalang.Common;    use Libadalang.Common;
with Langkit_Support.Text; use Langkit_Support.Text;

package body LAL_Refactor.Stub_Utils is

   function Detect_Indent (N : Ada_Node'Class) return Natural
   with Pre => not (N.Is_Null or else N.Parent.Is_Null);
   --  Detect indentation from context using node and parent node SLOC

   function Detect_Offset (Start_Pos : Source_Location_Range) return Natural;
   --  Detect starting offset from context using node SLOC

   function Signature
     (Self : Subp_Code_Generator'Class) return Unbounded_String;
   --  Builds string equivalent to declaration text

   -------------------
   -- Detect_Indent --
   -------------------

   function Detect_Indent (N : Ada_Node'Class) return Natural is
      Scope_Indent, Parent_Indent : Natural;
   begin
      Scope_Indent := Natural (N.Sloc_Range.Start_Column);
      Parent_Indent := Natural (N.P_Parent_Basic_Decl.Sloc_Range.Start_Column);
      return Scope_Indent - Parent_Indent;
   exception
      when E : others =>
         Refactor_Trace.Trace (E, "Could not detect indentation from context");
         return Default_Indent;
   end Detect_Indent;

   -------------------
   -- Detect_Offset --
   -------------------

   function Detect_Offset (Start_Pos : Source_Location_Range) return Natural is
   begin
      return Natural (Start_Pos.Start_Column - 1);
   exception
      when E : others =>
         Refactor_Trace.Trace
           (E, "Could not detect start indentation from context");
         return 0;
   end Detect_Offset;

   ---------------
   -- Signature --
   ---------------

   function Signature
     (Self : Subp_Code_Generator'Class) return Unbounded_String
   is
      T           : Token_Reference := Self.Decl.Token_Start;
      Decl_String : Unbounded_String;
   begin
      --  We do not iterate through the entire token range of Decl
      --  as this would include declaration-only details, e.g. aspects
      --  Instead, stop at the end of the subprogram specification
      loop
         exit when Self.Decl.F_Subp_Spec.Token_End < T;
         case Kind (Data (T)) is
            --  Replace tabs or linebreaks with a single space
            --  Then fast-forward through any excess whitespace
            when Ada_Whitespace =>
               Append (Decl_String, " ");
               T := T.Next (Exclude_Trivia => True);
            when Ada_Comment    =>
               T := T.Next (Exclude_Trivia => True);
            when others         =>
               Append (Decl_String, To_UTF8 (T.Text));
               T := T.Next (Exclude_Trivia => False);
         end case;
      end loop;
      return Decl_String;
   exception
      when E : others =>
         Refactor_Trace.Trace (E, "Failed to build subprogram signature");
         return Null_Unbounded_String;
   end Signature;

   ------------------------
   -- Build_Subunit_Body --
   ------------------------

   function Build_Subunit_Body
     (Name       : String;
      Signature  : String;
      Inner_Body : String;
      Offset     : Natural;
      Indent     : Natural) return Unbounded_String
   is
      Subunit_Body : Unbounded_String := To_Unbounded_String (Line_End);

      function Fmt_Stmt
        (Line : String; Nesting : Natural := 0) return Unbounded_String
      is (Fmt_Src (Line, Offset, Indent, Nesting, Stmt_Term => True));

      function Fmt_Decl
        (Line : String; Nesting : Natural := 0) return Unbounded_String
      is (Fmt_Src (Line, Offset, Indent, Nesting, Stmt_Term => False));

   begin
      Append (Subunit_Body, Fmt_Decl (Signature & " is"));
      Append (Subunit_Body, Fmt_Decl (Begin_Keyword));
      if Inner_Body /= "" then
         Append (Subunit_Body, Fmt_Stmt (Inner_Body, Nesting => 1));
      else
         Append (Subunit_Body, Fmt_Decl (Inner_Body, Nesting => 1));
      end if;
      Append (Subunit_Body, Fmt_Stmt (End_Keyword & " " & Name));
      return Subunit_Body;
   end Build_Subunit_Body;

   ------------------------------
   -- Generate_Subprogram_Body --
   ------------------------------

   function Generate_Subprogram_Body
     (Self : Subp_Code_Generator'Class) return Unbounded_String is
   begin
      return
        Build_Subunit_Body
          (Name       => To_String (Self.Name),
           Signature  => To_String (Self.Signature),
           Inner_Body => "",
           --  Do not prefill body; leave empty for cursor to move here
           Offset     => Detect_Offset (Self.Decl.Sloc_Range),
           Indent     => Detect_Indent (Self.Decl));
   exception
      when E : others =>
         Refactor_Trace.Trace (E, "Failed to build subprogram body text");
         return Null_Unbounded_String;
   end Generate_Subprogram_Body;

   ---------------------------
   -- Create_Code_Generator --
   ---------------------------

   function Create_Code_Generator
     (Decl : Subp_Decl) return Subp_Code_Generator is
   begin
      return
          (Name => To_Unbounded_String (To_UTF8 (Decl.P_Defining_Name.Text)),
           Decl => Decl);
   end Create_Code_Generator;
end LAL_Refactor.Stub_Utils;
