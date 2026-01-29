--
--  Copyright (C) 2025-2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with VSS.Characters;
with VSS.Characters.Latin;
with VSS.Strings.Cursors.Iterators.Characters;
use  VSS.Strings.Cursors.Iterators.Characters;

with Langkit_Support.Text; use Langkit_Support.Text;
with Libadalang.Common;    use Libadalang.Common;

package body LAL_Refactor.Stub_Utils is
   function In_Whitespace
     (X : VSS.Characters.Virtual_Character'Base) return Boolean;
   --  Helper function

   function Contains_Only_Whitespace
     (Item : Virtual_String'Class) return Boolean;
   --  True unless string contains non-whitespace characters
   --  True if line empty too

   function Trim_Right
     (Item : Virtual_String'Class) return Virtual_String'Class;
   --  Trim trailing whitespace from the right side of a string

   -------------------
   -- In_Whitespace --
   -------------------

   function In_Whitespace
     (X : VSS.Characters.Virtual_Character'Base) return Boolean
   is
      use VSS.Characters.Latin;
      type Virtual_Char_Array is
        array (Positive range 1 .. 3) of VSS.Characters.Virtual_Character'Base;
      Whitespace_Set : constant Virtual_Char_Array :=
        [Line_Feed, Carriage_Return, Space];
   begin
      return (for some Whitespace of Whitespace_Set => X in Whitespace);
   end In_Whitespace;

   ------------------------------
   -- Contains_Only_Whitespace --
   ------------------------------

   function Contains_Only_Whitespace
     (Item : Virtual_String'Class) return Boolean
   is
      S : Character_Iterator := Item.Before_First_Character;
      C : VSS.Characters.Virtual_Character'Base := S.Element;
   begin
      while S.Forward (C) loop
         if not In_Whitespace (C) then
            return False;
         end if;
      end loop;
      return True;
   end Contains_Only_Whitespace;

   ----------------
   -- Trim_Right --
   ----------------

   function Trim_Right
     (Item : Virtual_String'Class) return Virtual_String'Class
   is
      S : Character_Iterator := Item.After_Last_Character;
   begin
      while S.Backward loop
         exit when not In_Whitespace (S.Element);
      end loop;
      return (if S.Forward then Item.Head_Before (S) else Item);
   end Trim_Right;

   ------------------------
   -- Build_Subunit_Body --
   ------------------------

   function Build_Subunit_Body
     (Signature  : VSS_Vector;
      Inner_Body : String;
      End_Label  : String;
      Offset     : Natural;
      Indent     : Natural) return VSS.Strings.Virtual_String
   is
      Stub_Lines : VSS_Vector := Signature;

      function Fmt_Src
        (Line : Virtual_String; Nest : Natural := Default_Nesting)
         return Virtual_String
      is (To_VS (Padding (Offset, Indent, Nest)) & Line);
      --  Toggle different levels of indentation

   begin
      VSS.String_Vectors.Append
        (Stub_Lines, Fmt_Src (To_VS (Inner_Body), Nest => 1));
      VSS.String_Vectors.Append (Stub_Lines, Fmt_Src (To_VS (End_Label)));
      return Generated_Stub : Virtual_String do
         Generated_Stub :=
           Fmt_Src
             (Stub_Lines.Join_Lines
                (Terminator => VSS.Strings.LF, Terminate_Last => True));
         Generated_Stub.Prepend (VSS.Characters.Latin.Line_Feed);
      end return;
   end Build_Subunit_Body;

   ---------------------------
   -- Create_Code_Generator --
   ---------------------------

   function Create_Code_Generator
     (Subprogram : Subp_Decl) return Subp_Code_Generator
   is (Name => To_Unbounded_String (To_UTF8 (Subprogram.P_Defining_Name.Text)),
       Decl => Subprogram);

   -------------------
   -- Generate_Body --
   -------------------

   overriding
   function Generate_Body (Self : Subp_Code_Generator) return Unbounded_String
   is
      function Detect_Indent (N : Ada_Node'Class) return Natural
      with Pre => not (N.Is_Null or else N.Parent.Is_Null);
      --  Detect indentation from context using node SLOC

      function Detect_Offset
        (Start_Pos : Source_Location_Range) return Natural;
      --  Detect starting offset from context using node SLOC

      function Signature (Decl : Subp_Decl) return VSS_Vector;
      --  Cleanup subprogram declaration text for use in body
      --  (remove comments, preserve linebreaks)

      -------------------
      -- Detect_Indent --
      -------------------

      function Detect_Indent (N : Ada_Node'Class) return Natural is
         Scope_Indent, Parent_Indent : Natural;
      begin
         Scope_Indent := Natural (N.Sloc_Range.Start_Column);
         Parent_Indent :=
           Natural (N.P_Parent_Basic_Decl.Sloc_Range.Start_Column);
         return Scope_Indent - Parent_Indent;
      exception
         when others =>
            return Default_Indent;
      end Detect_Indent;

      -------------------
      -- Detect_Offset --
      -------------------

      function Detect_Offset (Start_Pos : Source_Location_Range) return Natural
      is
      begin
         return Natural (Start_Pos.Start_Column - 1);
      exception
         when others =>
            return 0;
      end Detect_Offset;

      ---------------
      -- Signature --
      ---------------

      function Signature (Decl : Subp_Decl) return VSS_Vector is

         --  Most of the relevant text for a subprogram declaration
         --  is found in the Subp_Spec node, but in special cases
         --  the parent Subp_Decl will include hierarchy modifiers
         --  such as "overriding"

         --  -------------------------------------------------------------
         --  |          subprogram.signature         |
         --  |---------------------------------------|--------------------
         --  |              procedure Print (I : T)    is abstract       |
         --  | overriding   procedure Print (I : Int)  with Pre => I > 0 |
         --  |            |--------------------------|                   |
         --  |            |        subp_spec         |                   |
         --  --------------------- subp_decl -----------------------------

         T          : Token_Reference := Decl.Token_Start;
         Decl_Text  : VSS.Strings.Virtual_String;
         Decl_Lines : VSS_Vector;
         Offset     : constant Natural := Detect_Offset (Decl.Sloc_Range);
         Indent     : constant Natural := Detect_Indent (Decl);
         Leading_WS : constant String := Padding (Offset, Indent, 0).To_String;

         I : Positive;
      begin
         --  Get text of the subprogram signature, skipping comments
         loop
            exit when Decl.F_Subp_Spec.Token_End < T;
            if T.Data.Kind not in Ada_Comment then
               Decl_Text.Append (VSS.Strings.To_Virtual_String (T.Text));
            end if;
            T := T.Next (Exclude_Trivia => False);
         end loop;

         --  Split at newlines (LF, CR, CRLF)
         Decl_Lines := Decl_Text.Split_Lines (Keep_Terminator => False);
         I := Decl_Lines.First_Index;

         --  Trim excess whitespace and remove empty lines
         while I <= Decl_Lines.Last_Index loop
            if Contains_Only_Whitespace (Decl_Lines (I)) then
               Decl_Lines.Delete (I);
            --  This moves the next element to current index

            else
               Decl_Lines.Replace (I, Trim_Right (Decl_Lines (I)));
               I := I + 1;
            end if;
         end loop;

         --  Add keywords
         Decl_Lines.Append (To_VS (Leading_WS & Keyword_Is));
         Decl_Lines.Append (To_VS (Leading_WS & Keyword_Begin));
         return Decl_Lines;
      exception
         when E : others =>
            Refactor_Trace.Trace (E, "Failed to build subprogram signature");
            return Empty_Virtual_String_Vector;
      end Signature;
   begin
      return
        From_VS
          (Build_Subunit_Body
             (Signature  => Signature (Self.Decl),
              Inner_Body => "",
              End_Label  => Self.End_Name,
              Offset     => Detect_Offset (Self.Decl.Sloc_Range),
              Indent     => Detect_Indent (Self.Decl)));
   exception
      when E : others =>
         Refactor_Trace.Trace (E, "Failed to build subprogram body text");
         return Null_Unbounded_String;
   end Generate_Body;

   ---------------------------
   -- Create_Code_Generator --
   ---------------------------

   function Create_Code_Generator
     (Spec : Base_Package_Decl; Subprograms : Decl_Vector)
      return Pkg_Code_Generator
   is (Name        => To_Unbounded_String (To_UTF8 (Spec.F_Package_Name.Text)),
       Subprograms => Subprograms);

   -------------------
   -- Generate_Body --
   -------------------

   overriding
   function Generate_Body (Self : Pkg_Code_Generator) return Unbounded_String
   is
      Signature : constant VSS_Vector :=
        To_Virtual_String_Vector
          (To_VS
             ((Keyword_Package_Body
               & " "
               & Self.Name.To_String
               & " "
               & Keyword_Is)));

      Subprogram_Block : Unbounded_String;
   begin
      --  Package body subprograms follow declaration order
      --  in package specification
      for D of Self.Subprograms loop
         declare
            Subprogram_Body : constant Unbounded_String :=
              Create_Code_Generator (D).Generate_Body;
         begin
            Subprogram_Block.Append (Subprogram_Body);
         end;
      end loop;
      return
        From_VS
          (Build_Subunit_Body
             (Signature  => Signature,
              Inner_Body => Subprogram_Block.To_String,
              End_Label  => Self.End_Name,
              Offset     => 0,
              Indent     => 0));
   exception
      when E : others =>
         Refactor_Trace.Trace (E, "Failed to build package body text");
         return Null_Unbounded_String;
   end Generate_Body;

end LAL_Refactor.Stub_Utils;
