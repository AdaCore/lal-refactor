--
--  Copyright (C) 2022-2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Characters.Wide_Wide_Latin_1;
with Ada.Strings.Wide_Wide_Unbounded;

with GNATCOLL.Traces; use GNATCOLL.Traces;

with Libadalang.Common; use Libadalang.Common;

package body LAL_Refactor.Sort_Dependencies is

   Me : constant Trace_Handle := Create ("LAL_REFACTOR.SORT_DEPENDENCIES");

   function Join
     (Vector    : Unbounded_Text_Vector;
      Separator : Unbounded_Text_Type)
      return Unbounded_Text_Type;
   --  Joins Vector elements with Separator

   function Join_With_Double_LF (Vector : Unbounded_Text_Vector)
      return Unbounded_Text_Type;
   --  Joins Vector elements with two LFs

   function Join_With_LF (Vector : Unbounded_Text_Vector)
      return Unbounded_Text_Type;
   --  Joins Vector elements with an LF

   function Leading_Comments
     (Node : Ada_Node'Class)
      return Unbounded_Text_Type;
   --  Gets the block of comments before Node up until a double line break is
   --  found.

   procedure Leading_Comments
     (Node        : Ada_Node'Class;
      Token_Start : out Token_Reference;
      Token_End   : out Token_Reference);
   --  Gets the block of comments before Node up until a double line break is
   --  found.

   function Is_Multi_Line_Break (Token : Token_Reference) return Boolean;
   --  Checks if Token is a Whitespace token with multiple line breaks

   function Is_Single_Line_Break (Token : Token_Reference) return Boolean;
   --  Checks if Token is a Whitespace token with multiple line breaks

   function Text
     (Token_Start, Token_End : Token_Reference)
      return Unbounded_Text_Type;
   --  Returns the text between Token_Start and Token_End inclusive.

   function Trailing_Comments
     (Node : Ada_Node'Class)
      return Unbounded_Text_Type;
   --  Gets the block of comments after Node up until the next Node is found
   --  or it's exceptional leading comments.

   procedure Trailing_Comments
     (Node        : Ada_Node'Class;
      Token_Start : out Token_Reference;
      Token_End   : out Token_Reference);
   --  Gets the block of comments after Node up until the next Node is found
   --  or it's exceptional leading comments.

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Packages_Clause_Type) return Boolean
   is
   begin
      --  Left.Packages < Right.Packages, where packages are compared
      --  sequentially.

      declare
         Left_Index  : Unbounded_Text_Vectors.Extended_Index :=
           Left.Packages.First_Index;
         Right_Index : Unbounded_Text_Vectors.Extended_Index :=
           Right.Packages.First_Index;

      begin
         loop
            if Left.Packages.Element (Left_Index)
               /= Right.Packages.Element (Right_Index)
            then
               return
                 Ada.Strings.Wide_Wide_Unbounded."<"
                   (Left.Packages.Element (Left_Index),
                    Right.Packages.Element (Right_Index));
            end if;

            if Left_Index = Left.Packages.Last_Index then
               --  Left has the same but less packages than Right
               return True;
            end if;

            if Right_Index = Right.Packages.Last_Index then
               --  Right has the same but less packages than Less
               return False;
            end if;

            Left_Index := Left_Index + 1;
            Right_Index := Right_Index + 1;
         end loop;
      end;
   end "<";

   --------------------
   -- Accept_Visitor --
   --------------------

   overriding
   procedure Accept_Visitor
     (Self : Use_Package_Clause_Type; Visitor : in out Clause_Visitor'Class)
   is
   begin
      Visitor.Visit (Self);
   end Accept_Visitor;

   --------------------
   -- Accept_Visitor --
   --------------------

   overriding
   procedure Accept_Visitor
     (Self    : With_Clause_Type;
      Visitor : in out Clause_Visitor'Class)
   is
   begin
      Visitor.Visit (Self);
   end Accept_Visitor;

   ---------------------
   -- Add_With_Clause --
   ---------------------

   procedure Add_With_Clause
     (Self        : in out Prelude_Section_Type;
      With_Clause : With_Clause_Type'Class)
   is
   begin
      Self.Clauses.Append (With_Clause_Type (With_Clause));

      for Withed_Package of With_Clause.Node.As_With_Clause.F_Packages loop
         Self.Packages.Include (To_Unbounded_Text (Withed_Package.Text));
      end loop;
   end Add_With_Clause;

   -----------------------------------
   -- Associate_Use_Package_Clauses --
   -----------------------------------

   procedure Associate_Use_Package_Clauses
     (Self                : in out Prelude_Section_Type;
      Use_Package_Clauses : in out Use_Package_Clause_Vector)
   is
      --  Warning:
      --  This algorithm is a naive search, with time complexity of O(n*m)
      --  where n = Self.Clauses.Length and m = Use_Package_Clauses.Length.
      --  This can be improved to O(n) by not using a vector to store the
      --  use clauses. However, that increases the implementation complexity
      --  quite a bit because access types would need to be introduced.
      --  Preludes are usually small, so O(n*m) seems acceptible for now.

      With_Clause_Cursor : With_Clause_Vectors.Cursor := Self.Clauses.First;

   begin
      while With_Clause_Vectors.Has_Element (With_Clause_Cursor) loop
         declare
            With_Clause_Reference :
              constant With_Clause_Vectors.Reference_Type :=
                Self.Clauses.Reference (With_Clause_Cursor);

         begin
            With_Clause_Loop :
            for With_Package of With_Clause_Reference.Packages loop
               declare
                  Use_Package_Clause_Cursor :
                    Use_Package_Clause_Vectors.Cursor :=
                      Use_Package_Clauses.First;

               begin
                  while Use_Package_Clause_Vectors.Has_Element
                          (Use_Package_Clause_Cursor)
                  loop
                     if Use_Package_Clause_Vectors
                          .Element (Use_Package_Clause_Cursor)
                          .Packages
                          .Contains (With_Package)
                     then
                        With_Clause_Reference
                          .Associated_Use_Package_Clauses
                          .Append
                             (Use_Package_Clause_Vectors.Element
                                (Use_Package_Clause_Cursor));
                        Use_Package_Clauses.Delete (Use_Package_Clause_Cursor);
                        Use_Package_Clause_Cursor := Use_Package_Clauses.First;

                     else
                        Use_Package_Clause_Vectors.Next
                          (Use_Package_Clause_Cursor);
                     end if;
                  end loop;
               end;
            end loop With_Clause_Loop;
         end;

         With_Clause_Vectors.Next (With_Clause_Cursor);
      end loop;
   end Associate_Use_Package_Clauses;

   -----------------------------------
   -- Associate_Use_Package_Clauses --
   -----------------------------------

   procedure Associate_Use_Package_Clauses (Self : in out Prelude_Type)
   is
   begin
      Self.Public_Section.Associate_Use_Package_Clauses
        (Self.Leftover_Use_Section);
      Self.Private_Section.Associate_Use_Package_Clauses
        (Self.Leftover_Use_Section);
      Self.Limited_Section.Associate_Use_Package_Clauses
        (Self.Leftover_Use_Section);
      Self.Limited_Private_Section.Associate_Use_Package_Clauses
        (Self.Leftover_Use_Section);
   end Associate_Use_Package_Clauses;

   --------------
   -- Contains --
   --------------

   function Contains
     (Self         : Prelude_Section_Type;
      Package_Name : Unbounded_Text_Type)
      return Boolean
   is (Self.Packages.Contains (Package_Name));

   --------------------------------
   -- Create_Dependencies_Sorter --
   --------------------------------

   function Create_Dependencies_Sorter
     (Compilation_Unit : Libadalang.Analysis.Compilation_Unit;
      No_Separator     : Boolean := True)
      return Dependencies_Sorter
   is (Dependencies_Sorter'
         (Prelude_Node               => Compilation_Unit.F_Prelude,
          Prelude_Clause_Start_Index =>
            Compilation_Unit.F_Prelude.First_Child_Index,
          Prelude_Clause_End_Index   =>
            Compilation_Unit.F_Prelude.Last_Child_Index,
          No_Separator               => No_Separator));

   --------------------------------
   -- Create_Dependencies_Sorter --
   --------------------------------

   function Create_Dependencies_Sorter
     (Compilation_Unit : Libadalang.Analysis.Compilation_Unit;
      Where            : Source_Location_Range;
      No_Separator     : Boolean := True)
      return Dependencies_Sorter
   is
      Prelude_Node : constant Ada_Node_List := Compilation_Unit.F_Prelude;

      function Get_Prelude_Clause
        (Location : Source_Location)
         return Ada_Node;
      --  Lookup the prelude clause that contains Location. Returns No_Ada_Node
      --  is non was found.

      ------------------------
      -- Get_Prelude_Clause --
      ------------------------

      function Get_Prelude_Clause
        (Location : Source_Location)
         return Ada_Node
      is
         Bottom_Most_Node : constant Ada_Node :=
           Compilation_Unit.Lookup (Location);
         Previous_Child   : Ada_Node          :=  Bottom_Most_Node;

      begin
         if Bottom_Most_Node.Is_Null then
            return No_Ada_Node;
         end if;

         --  Given the pre condition of Create_Dependencies_Sorter (``Where``
         --  must be inside ``Compilation_Unit.F_Prelude``), we should always
         --  hit the return inside the loop.
         for Node of Bottom_Most_Node.Parents (With_Self => False) loop
            if Node = Prelude_Node then
               return Previous_Child;
            end if;
            Previous_Child := Node;
         end loop;

         return No_Ada_Node;
      end Get_Prelude_Clause;

      Start_Prelude_Clause : constant Ada_Node :=
        Get_Prelude_Clause (Where.Start_Sloc);
      End_Prelude_Clause   : constant Ada_Node :=
        Get_Prelude_Clause (Where.End_Sloc);

   begin
      if Start_Prelude_Clause.Is_Null or End_Prelude_Clause.Is_Null then
         raise Program_Error with "Failed to get prelude clause";
      end if;

      if Start_Prelude_Clause = End_Prelude_Clause then
         return Create_Dependencies_Sorter (Compilation_Unit, No_Separator);
      end if;

      declare
         --  Node.Child_Index returns the 0-based index for Node in its
         --  parent's children. Both
         --  Dependencies_Sorter.Prelude_Clause_Start_Index and
         --  Dependencies_Sorter.Prelude_Clause_End_Index expect a 1-based
         --  index so that it's compatible with Ada_Node_List (the kind of node
         --  the prelude is).
         Prelude_Clause_Start_Index : constant Positive :=
           Start_Prelude_Clause.Child_Index + 1;
         Prelude_Clause_End_Index   : constant Positive :=
           End_Prelude_Clause.Child_Index + 1;

      begin
         return
           Dependencies_Sorter'
             (Prelude_Node,
              Prelude_Clause_Start_Index,
              Prelude_Clause_End_Index,
              No_Separator);
      end;
   end Create_Dependencies_Sorter;

   ----------
   -- Join --
   ----------

   function Join
     (Vector    : Unbounded_Text_Vector;
      Separator : Unbounded_Text_Type)
      return Unbounded_Text_Type
   is
      use Ada.Strings.Wide_Wide_Unbounded;
      use Unbounded_Text_Vectors;

      Result : Unbounded_Text_Type;

      Element_Cursor : Cursor := Vector.First;

   begin
      if not Has_Element (Element_Cursor) then
         return Result;
      end if;

      Append (Result, Element (Element_Cursor));
      Next (Element_Cursor);
      while Has_Element (Element_Cursor) loop
         Append (Result, Separator);
         Append (Result, Element (Element_Cursor));
         Next (Element_Cursor);
      end loop;

      return Result;
   end Join;

   -------------------------
   -- Join_With_Double_LF --
   -------------------------

   function Join_With_Double_LF (Vector : Unbounded_Text_Vector)
      return Unbounded_Text_Type
   is
      use Ada.Characters.Wide_Wide_Latin_1;
      use Ada.Strings.Wide_Wide_Unbounded;

   begin
      return Join (Vector, 2 * LF);
   end Join_With_Double_LF;

   ------------------
   -- Join_With_LF --
   ------------------

   function Join_With_LF (Vector : Unbounded_Text_Vector)
      return Unbounded_Text_Type
   is
      use Ada.Characters.Wide_Wide_Latin_1;
      use Ada.Strings.Wide_Wide_Unbounded;

   begin
      return Join (Vector, 1 * LF);
   end Join_With_LF;

   ----------------------
   -- Leading_Comments --
   ----------------------

   function Leading_Comments
     (Node : Ada_Node'Class)
      return Unbounded_Text_Type
   is
      Token_Start : Token_Reference;
      Token_End   : Token_Reference;

   begin
      Leading_Comments (Node, Token_Start, Token_End);

      return Text (Token_Start, Token_End);
   end Leading_Comments;

   ----------------------
   -- Leading_Comments --
   ----------------------

   procedure Leading_Comments
     (Node        : Ada_Node'Class;
      Token_Start : out Token_Reference;
      Token_End   : out Token_Reference)
   is
      Node_Token_Start : constant Token_Reference := Node.Token_Start;
      Current_Token    : Token_Reference          :=
        Previous (Node_Token_Start);

      Comment_Seen          : Boolean := False;
      Multi_Line_Break_Seen : Boolean := False;

   begin
      Token_Start := No_Token;
      Token_End := No_Token;

      if Current_Token = No_Token then
         return;
      else
         Token_End := Current_Token;
      end if;

      loop
         exit when Current_Token = No_Token
           or else Kind (Data (Current_Token))
                   not in Ada_Whitespace | Ada_Comment;
         Comment_Seen := @ or Kind (Data (Current_Token)) in Ada_Comment;
         if Is_Multi_Line_Break (Current_Token) then
            Multi_Line_Break_Seen := True;
            exit;
         elsif Is_Single_Line_Break (Current_Token)
           and then Kind (Data (Previous (Current_Token))) in Ada_Comment
         then
            Token_Start := Current_Token;
         else
            Token_Start := Current_Token;
         end if;
         Current_Token := Previous (Current_Token);
      end loop;

      if not Comment_Seen or else not Multi_Line_Break_Seen then
         Token_Start := No_Token;
         Token_End := No_Token;
      end if;
   end Leading_Comments;

   -------------------------
   -- Is_Multi_Line_Break --
   -------------------------

   function Is_Multi_Line_Break (Token : Token_Reference) return Boolean is
     (Kind (Data (Token)) in Ada_Whitespace
      and then Sloc_Range (Data (Token)).End_Line
               - Sloc_Range (Data (Token)).Start_Line
               > 1);

   --------------------------
   -- Is_Single_Line_Break --
   --------------------------

   function Is_Single_Line_Break (Token : Token_Reference) return Boolean is
     (Kind (Data (Token)) in Ada_Whitespace
      and then Sloc_Range (Data (Token)).End_Line
               - Sloc_Range (Data (Token)).Start_Line
               = 1);

   ------------------------------------
   -- Is_Sort_Dependencies_Available --
   ------------------------------------

   function Is_Sort_Dependencies_Available
     (Unit             : Analysis_Unit;
      Sloc             : Source_Location)
      return Boolean is
   begin
      if Unit = No_Analysis_Unit or else Unit.Root.Is_Null then
         return False;
      end if;

      declare
         Node                  : constant Ada_Node := Unit.Root.Lookup (Sloc);
         Node_Compilation_Unit : Libadalang.Analysis.Compilation_Unit;
         Node_Prelude          : Ada_Node_List;

      begin
         if Node.Is_Null then
            return False;
         end if;

         Node_Compilation_Unit := Node.P_Enclosing_Compilation_Unit;
         if Node_Compilation_Unit.Is_Null then
            return False;
         end if;

         Node_Prelude := Node_Compilation_Unit.F_Prelude;
         if Node_Prelude.Is_Null then
            return False;
         end if;

         for Prelude_Node of Node_Prelude loop
            if Prelude_Node.Is_Null
              or else Prelude_Node.Kind
                      not in Ada_With_Clause_Range
                             | Ada_Use_Package_Clause_Range
                             | Ada_Pragma_Node_Range
            then
               return False;
            end if;
         end loop;

         return Compare (Node_Prelude.Sloc_Range, Sloc) = Inside;
      end;
   end Is_Sort_Dependencies_Available;

   ------------------------------------
   -- Is_Sort_Dependencies_Available --
   ------------------------------------

   function Is_Sort_Dependencies_Available
     (Unit      : Analysis_Unit;
      Selection : Source_Location_Range)
      return Boolean
   is (Is_Sort_Dependencies_Available (Unit, Selection.Start_Sloc)
       and (Selection.Start_Sloc = Selection.End_Sloc
            or Is_Sort_Dependencies_Available (Unit, Selection.End_Sloc)));

   --------------
   -- Refactor --
   --------------

   overriding
   function Refactor
     (Self           : Dependencies_Sorter;
      Analysis_Units : access function return Analysis_Unit_Array)
      return Refactoring_Edits
   is
      Edits : Refactoring_Edits;

      procedure Process_Prelude
        (Start_Clause_Index : Positive;
         End_Clause_Index   : Positive);
      --  Steps:
      --  1) Skip initial pragma list (this does not need to be sorted)
      --  2) Processes each remaining node of the prelude by building a
      --     Prelude_Type object
      --  3) Renders the Prelude_Type object and adds it to Edits

      ---------------------
      -- Process_Prelude --
      ---------------------

      procedure Process_Prelude
        (Start_Clause_Index : Positive;
         End_Clause_Index   : Positive)
      is
         Adjusted_Start_Clause_Index : Positive := Start_Clause_Index;
         --  First and last node of the prelude that will be sorted

         Current_Clause : Packages_Clause_Access := null;

         Prelude : Prelude_Type;

         procedure Compute_Edits;
         --  Sorts and renders the Prelude_Type object and adds it to Edits.
         --  The edits source location range is given by Initial_Node and
         --  Final_Node_Non_Inclusive.

         procedure Process_Prelude_Clause
           (Prelude_Clause : Libadalang.Analysis.Ada_Node);
         --  If `Prelude_Clause` is a `WithClause` or `UsePackageClause`:
         --     1) if `Current_Clause /= null` then `Current_Clause` is
         --        is considered as finished and is added to `Prelude`
         --     2) rebuilds `Current_Clause` based on `Prelude_Node`
         --  If `Prelude_Node` is a `UsePackageClause`:
         --     1) builds a `Pragma_Clause_Type` object and adds it to
         --        `Current_Clause`

         procedure Skip_Initial_Pragma_List;
         --  If the predule starts with pragma nodes, these don't need to be
         --  sorted. Then skip these.

         -------------------
         -- Compute_Edits --
         -------------------

         procedure Compute_Edits is
            use Ada.Strings.Wide_Wide_Unbounded;

            function Compute_End_Location return Source_Location;
            --  Computes the end location of the text edit.
            --  If Self.Prelude_Clause_End_Index is the last clause of the
            --  prelude, then this is the the start location of the unit body
            --  or of its leading comments if existent.
            --  Otherwise, it is the start location of the token after the
            --  clause defined by Self.Prelude_Clause_End_Index.

            function Compute_Start_Location return Source_Location;
            --  Computes the start location of the text edit.
            --  This is the start location of the first clause or of its
            --  leading comments if existent.

            --------------------------
            -- Compute_End_Location --
            --------------------------

            function Compute_End_Location return Source_Location
            is
               Token_End   : Token_Reference := No_Token;
               Token_Dummy : Token_Reference := No_Token;

               End_Node : constant Ada_Node'Class :=
                 Ada_Node_List_Element (Self.Prelude_Node, End_Clause_Index);

            begin
               Trailing_Comments (End_Node, Token_Dummy, Token_End);

               --  If End_Node has trailing comments, then Token_End is the
               --  text edit last token. Otherwise use End_Node.Token_End.

               if Token_End = No_Token then
                  Token_End := End_Node.Token_End;
               end if;

               --  The end location of the text edit is non inclusive.
               --  Therefore, use the start location of the token after
               --  Token_End.
               return Start_Sloc (Sloc_Range (Data (Next (Token_End))));
            end Compute_End_Location;

            ----------------------------
            -- Compute_Start_Location --
            ----------------------------

            function Compute_Start_Location return Source_Location
            is
               Token_Start : Token_Reference := No_Token;
               Token_Dummy : Token_Reference := No_Token;

               Start_Node  : constant Ada_Node'Class :=
                 Ada_Node_List_Element
                   (Self.Prelude_Node, Adjusted_Start_Clause_Index);

            begin
               Leading_Comments
                 (Start_Node, Token_Start, Token_Dummy);
               --  If Start_Node has leading comments, then Token_Start is the
               --  text edit first token.

               return
                 (if Token_Start /= No_Token then
                    Start_Sloc (Sloc_Range (Data (Token_Start)))
                  else
                    Start_Sloc (Start_Node.Sloc_Range));
               --  If Token_Start /= No_Token then Start_Node has leading
               --  comments. Use Token_Start start location as the text edit
               --  initial location. Otherwise use Start_Node start location.
            end Compute_Start_Location;

            Prelude_Text : Unbounded_Text_Type;

         begin
            Me.Trace ("Computing refactoring edits");

            Prelude.Associate_Use_Package_Clauses;
            Prelude.Sort;
            Prelude_Text := To_Ada_Source (Prelude, Self.No_Separator);

            declare
               Edit_SLOC_Range  : constant Source_Location_Range :=
                 Make_Range (Compute_Start_Location, Compute_End_Location);

               Edit : constant Text_Edit :=
                 Text_Edit'
                   (Location => Edit_SLOC_Range,
                    Text     =>
                      To_Unbounded_String (To_UTF8 (To_Text (Prelude_Text))));

            begin
               Safe_Insert
                 (Edits.Text_Edits,
                  Self.Prelude_Node.Unit.Get_Filename, Edit);
            end;
         end Compute_Edits;

         ----------------------------
         -- Process_Prelude_Clause --
         ----------------------------

         procedure Process_Prelude_Clause
           (Prelude_Clause : Libadalang.Analysis.Ada_Node)
         is
            function First_Name
              (Name : Libadalang.Analysis.Name)
               return Unbounded_Text_Type
            is (if Name.Kind in Ada_Dotted_Name_Range
                then First_Name (Name.As_Dotted_Name.F_Prefix)
                else To_Unbounded_Text (Name.Text));
            --  Returns the first name of `Name` if it's a `Dotted_Name`
            --  otherwise returns `Name`.

            Leading_Comments  : constant Unbounded_Text_Type :=
              LAL_Refactor
                .Sort_Dependencies
                .Leading_Comments (Prelude_Clause);
            Trailing_Comments : constant Unbounded_Text_Type :=
              LAL_Refactor
                .Sort_Dependencies
                .Trailing_Comments (Prelude_Clause);

         begin
            if Prelude_Clause.Kind in Ada_With_Clause_Range then
               if Current_Clause /= null then
                  Current_Clause.Accept_Visitor (Prelude);
                  Free (Current_Clause);
                  Current_Clause := null;
               end if;

               declare
                  With_Clause              :
                    constant Libadalang.Analysis.With_Clause                :=
                      Prelude_Clause.As_With_Clause;
                  Packages                 : constant Unbounded_Text_Vector :=
                    [for Package_Name of With_Clause.F_Packages
                     => To_Unbounded_Text (Package_Name.Text)];
                  First_Package            : constant Name                  :=
                    With_Clause.F_Packages.First_Child.As_Name;
                  Is_Dotted_Name           : constant Boolean               :=
                    First_Package.Kind in Ada_Dotted_Name;
                  First_Package_First_Name : constant Unbounded_Text_Type   :=
                    First_Name (First_Package);

               begin
                  Current_Clause :=
                    new With_Clause_Type'
                          (Node                     => Prelude_Clause,
                           Is_Dotted_Name           => Is_Dotted_Name,
                           Packages                 => Packages,
                           First_Package_First_Name =>
                             First_Package_First_Name,
                           Leading_Comments         => Leading_Comments,
                           Trailing_Comments        => Trailing_Comments,
                           Associated_Pragmas       =>
                             Pragma_Clause_Vectors.Empty_Vector,
                           Associated_Use_Package_Clauses   =>
                             Use_Package_Clause_Vectors.Empty_Vector);
               end;

            elsif Prelude_Clause.Kind in Ada_Use_Package_Clause_Range then
               if Current_Clause /= null then
                  Current_Clause.Accept_Visitor (Prelude);
                  Free (Current_Clause);
                  Current_Clause := null;
               end if;

               declare
                  Use_Package_Clause       :
                    constant Libadalang.Analysis.Use_Package_Clause         :=
                      Prelude_Clause.As_Use_Package_Clause;
                  Packages                 : constant Unbounded_Text_Vector :=
                    [for Package_Name of Use_Package_Clause.F_Packages
                     => To_Unbounded_Text (Package_Name.Text)];
                  First_Package            : constant Name                  :=
                     Use_Package_Clause.F_Packages.First_Child.As_Name;
                  Is_Dotted_Name           : constant Boolean               :=
                    First_Package.Kind in Ada_Dotted_Name;
                  First_Package_First_Name : constant Unbounded_Text_Type   :=
                    First_Name (First_Package);

               begin
                  Current_Clause :=
                    new Use_Package_Clause_Type'
                          (Node                     => Prelude_Clause,
                           Is_Dotted_Name           => Is_Dotted_Name,
                           Packages                 => Packages,
                           First_Package_First_Name =>
                             First_Package_First_Name,
                           Leading_Comments         => Leading_Comments,
                           Trailing_Comments        => Trailing_Comments,
                           Associated_Pragmas       =>
                             Pragma_Clause_Vectors.Empty_Vector);
               end;

            elsif Prelude_Clause.Kind in Ada_Pragma_Node_Range then
               declare
                  Clause : constant Pragma_Clause_Type :=
                    Pragma_Clause_Type'
                      (Prelude_Clause, Leading_Comments, Trailing_Comments);

               begin
                  Current_Clause.Associated_Pragmas.Append (Clause);
               end;

            else
               --  Reaching this case should be impossble since only
               --  WithClause, UsePackageClause and PragmaNode nodes should be
               --  in the prelude.
               raise Program_Error
                 with "Unexpected prelude node kind "
                      & Prelude_Clause.Kind_Name;

            end if;
         end Process_Prelude_Clause;

         ------------------------------
         -- Skip_Initial_Pragma_List --
         ------------------------------

         procedure Skip_Initial_Pragma_List
         is
         begin
            Me.Trace ("Skipping the initial pragma list");

            while Ada_Node_List_Has_Element
                    (Self.Prelude_Node, Adjusted_Start_Clause_Index)
            loop
               exit when
                 Ada_Node_List_Element
                   (Self.Prelude_Node, Adjusted_Start_Clause_Index)
                   .Kind
                 not in Ada_Pragma_Node_Range
                 or Adjusted_Start_Clause_Index >= End_Clause_Index;

               Me.Trace
                 ("Skipping "
                  & Ada_Node_List_Element
                      (Self.Prelude_Node, Adjusted_Start_Clause_Index)
                      .Image);

               Adjusted_Start_Clause_Index :=
                 Ada_Node_List_Next
                   (Self.Prelude_Node, Adjusted_Start_Clause_Index);
            end loop;
         end Skip_Initial_Pragma_List;

      begin
         Skip_Initial_Pragma_List;

         if not Ada_Node_List_Has_Element
                  (Self.Prelude_Node, Adjusted_Start_Clause_Index)
            or Adjusted_Start_Clause_Index >= End_Clause_Index
         then
            Me.Trace
              ("All the prelude elements have been skipped");

            return;
         end if;

         Me.Trace ("Initial node index:" & Adjusted_Start_Clause_Index'Image);
         Me.Trace ("Final node index: " & End_Clause_Index'Image);

         for Prelude_Clause_Index in
           Adjusted_Start_Clause_Index .. End_Clause_Index
         loop
            Me.Trace
              ("Process prelude clause "
               & Ada_Node_List_Element
                   (Self.Prelude_Node, Prelude_Clause_Index)
                   .Image);

            Process_Prelude_Clause
              (Ada_Node_List_Element (Self.Prelude_Node, Prelude_Clause_Index)
                 .As_Ada_Node);
         end loop;

         pragma Assert (Current_Clause /= null);

         Current_Clause.Accept_Visitor (Prelude);
         Free (Current_Clause);

         Me.Trace ("Finished processing prelude");

         Compute_Edits;
      end Process_Prelude;

   begin
      Process_Prelude
        (Self.Prelude_Clause_Start_Index,
         Self.Prelude_Clause_End_Index);

      return Edits;
   end Refactor;

   -------------------
   -- To_Ada_Source --
   -------------------

   function To_Ada_Source
     (Self : Pragma_Clause_Vector) return Unbounded_Text_Type
   is
      Pragma_Clauses : constant Unbounded_Text_Vector :=
        [for Pragma_Clause of Self => Pragma_Clause.To_Ada_Source];

   begin
      return Join_With_LF (Pragma_Clauses);
   end To_Ada_Source;

   -------------------
   -- To_Ada_Source --
   -------------------

   function To_Ada_Source
     (Self : Pragma_Clause_Type)
      return Unbounded_Text_Type
   is
      use Ada.Strings.Wide_Wide_Unbounded;

   begin
      return Result : Unbounded_Text_Type := Null_Unbounded_Wide_Wide_String do
         Append (Result, Self.Leading_Comments);
         Append (Result, Self.Node.Text);
         Append (Result, Self.Trailing_Comments);
      end return;
   end To_Ada_Source;

   -------------------
   -- To_Ada_Source --
   -------------------

   function To_Ada_Source
     (Self         : Prelude_Section_Type;
      No_Separator : Boolean := True)
      return Unbounded_Text_Type
   is
      use Ada.Strings.Wide_Wide_Unbounded;

      Result : Unbounded_Text_Type := Null_Unbounded_Wide_Wide_String;

   begin
      if Self.Clauses.Is_Empty then
         return Result;
      end if;

      declare
         Clauses_Cursor : With_Clause_Vectors.Cursor := Self.Clauses.First;

         First_Element :
           constant With_Clause_Vectors.Constant_Reference_Type :=
             With_Clause_Vectors.Constant_Reference
               (Self.Clauses, Clauses_Cursor);

      begin
         Append (Result, First_Element.To_Ada_Source);

         With_Clause_Vectors.Next (Clauses_Cursor);

         while With_Clause_Vectors.Has_Element (Clauses_Cursor) loop
            Append (Result, Ada.Characters.Wide_Wide_Latin_1.LF);

            declare
               Element          :
                 constant With_Clause_Vectors.Constant_Reference_Type :=
                   With_Clause_Vectors.Constant_Reference
                     (Self.Clauses, Clauses_Cursor);
               Previous_Element :
                 constant With_Clause_Vectors.Constant_Reference_Type :=
                   With_Clause_Vectors.Constant_Reference
                     (Self.Clauses,
                      With_Clause_Vectors.Previous (Clauses_Cursor));

            begin
               --  Add additional blank lines to separate groups depending on
               --  No_Separator.
               if not No_Separator
                  and then
                    not Langkit_Support.Text."="
                          (Previous_Element
                             .First_Package_First_Name,
                           Element.First_Package_First_Name)
               then
                  Append (Result, Ada.Characters.Wide_Wide_Latin_1.LF);
               end if;

               Append (Result, Element.To_Ada_Source);

               With_Clause_Vectors.Next (Clauses_Cursor);
            end;
         end loop;
      end;

      return Result;
   end To_Ada_Source;

   -------------------
   -- To_Ada_Source --
   -------------------

   function To_Ada_Source
     (Self         : Prelude_Type;
      No_Separator : Boolean := True)
      return Unbounded_Text_Type
   is
      use Ada.Strings.Wide_Wide_Unbounded;

      All_Sections : constant Unbounded_Text_Vector :=
        [To_Ada_Source (Self.Public_Section, No_Separator),
         To_Ada_Source (Self.Private_Section, No_Separator),
         To_Ada_Source (Self.Limited_Section, No_Separator),
         To_Ada_Source (Self.Limited_Private_Section, No_Separator)];

      --  NOTE: The following spec is commented due to a GNAT bug
      --  function Filter_Empty (Sections : Unbounded_Text_Vector)
      --    return Unbounded_Text_Vector;
      --  Returns Sections without Null_Unbounded_Wide_Wide_String elements

      -------------------
      --  Filter_Empty --
      -------------------

      function Filter_Empty (Sections : Unbounded_Text_Vector)
         return Unbounded_Text_Vector
      is ([for Section of Sections
           when not Langkit_Support.Text."="
                      (Section, Null_Unbounded_Wide_Wide_String)
           => Section]);

   begin
      return Join_With_Double_LF (Filter_Empty (All_Sections));
   end To_Ada_Source;

   -------------------
   -- To_Ada_Source --
   -------------------

   function To_Ada_Source
     (Self : Use_Package_Clause_Vector) return Unbounded_Text_Type
   is
      Use_Package_Clauses : constant Unbounded_Text_Vector :=
        [for Use_Package_Clause of Self => Use_Package_Clause.To_Ada_Source];

   begin
      return Join_With_LF (Use_Package_Clauses);
   end To_Ada_Source;

   -------------------
   -- To_Ada_Source --
   -------------------

   function To_Ada_Source
     (Self : Use_Package_Clause_Type)
      return Unbounded_Text_Type
   is
      use Ada.Strings.Wide_Wide_Unbounded;

   begin
      return Result : Unbounded_Text_Type := Null_Unbounded_Wide_Wide_String do
         Append (Result, Self.Leading_Comments);
         Append (Result, Self.Node.Text);
         Append (Result, Self.Trailing_Comments);

         --  Then process the pragmas associated to this clause
         for Associated_Pragma of Self.Associated_Pragmas loop
            Append (Result, Ada.Characters.Wide_Wide_Latin_1.LF);
            Append (Result, Associated_Pragma.To_Ada_Source);
         end loop;
      end return;
   end To_Ada_Source;

   -------------------
   -- To_Ada_Source --
   -------------------

   function To_Ada_Source
     (Self : With_Clause_Type)
      return Unbounded_Text_Type
   is
      use Ada.Strings.Wide_Wide_Unbounded;
      use type Ada.Containers.Count_Type;

   begin
      return Result : Unbounded_Text_Type := Null_Unbounded_Wide_Wide_String do
         Append (Result, Self.Leading_Comments);
         Append (Result, Self.Node.Text);
         Append (Result, Self.Trailing_Comments);

         --  Here we need to decide if we add a line break after the with
         --  clause. Sometimes we want to have the use clause on the same
         --  line as the with clause, for instance:
         --
         --  with Foo; use Foo;
         --
         --  All the following conditions need to be met in order to put the
         --  Use_Package_Clause on the same line as the With_Clause:
         --     - The with clause does not have trailing comments
         --     - The with clause only has one package
         --     - The with clause only has one associated Use_Package_Clause
         --     - The associated Use_Package_Clause only has one package
         --     - The associated Use_Package_Clause does not have leading
         --       comments
         if Langkit_Support.Text."="
              (Self.Trailing_Comments, Null_Unbounded_Wide_Wide_String)
           and then Self.Packages.Length = 1
           and then Self.Associated_Use_Package_Clauses.Length = 1
           and then Self
                     .Associated_Use_Package_Clauses
                     .First_Element
                     .Packages
                     .Length = 1
           and then Langkit_Support.Text."="
                      (Self
                         .Associated_Use_Package_Clauses
                         .First_Element
                         .Leading_Comments,
                       Null_Unbounded_Wide_Wide_String)
         then
            Append (Result, Ada.Characters.Wide_Wide_Latin_1.Space);

         elsif not Self.Associated_Use_Package_Clauses.Is_Empty then
            Append (Result, Ada.Characters.Wide_Wide_Latin_1.LF);
         end if;
         --  Then process the Use_Package_Clauses associated to this clause
         Append (Result, To_Ada_Source (Self.Associated_Use_Package_Clauses));

         if not Self.Associated_Pragmas.Is_Empty then
            Append (Result, Ada.Characters.Wide_Wide_Latin_1.LF);
         end if;
         --  Then process the pragmas associated to this clause
         Append (Result, To_Ada_Source (Self.Associated_Pragmas));

      end return;
   end To_Ada_Source;

   ----------
   -- Sort --
   ----------

   procedure Sort (Self : in out Prelude_Section_Type)
   is
      With_Clause_Cursor : With_Clause_Vectors.Cursor := Self.Clauses.First;

   begin
      while With_Clause_Vectors.Has_Element (With_Clause_Cursor) loop
         declare
            With_Clause_Reference :
              constant With_Clause_Vectors.Reference_Type :=
                Self.Clauses.Reference (With_Clause_Cursor);

         begin
            Use_Package_Clause_Vectors_Sorting.Sort
              (With_Clause_Reference.Associated_Use_Package_Clauses);
         end;

         With_Clause_Vectors.Next (With_Clause_Cursor);
      end loop;

      With_Clause_Vectors_Sorting.Sort (Self.Clauses);
   end Sort;

   ----------
   -- Sort --
   ----------

   procedure Sort (Self : in out Prelude_Type)
   is
   begin
      Self.Public_Section.Sort;
      Self.Private_Section.Sort;
      Self.Limited_Section.Sort;
      Self.Limited_Private_Section.Sort;
      Use_Package_Clause_Vectors_Sorting.Sort (Self.Leftover_Use_Section);
   end Sort;

   ----------
   -- Text --
   ----------

   function Text
     (Token_Start, Token_End : Token_Reference)
      return Unbounded_Text_Type
   is
      use Ada.Strings.Wide_Wide_Unbounded;

      Result : Unbounded_Text_Type := Null_Unbounded_Wide_Wide_String;

      Current_Token : Token_Reference := Token_Start;

   begin
      if Token_Start = No_Token or else Token_End = No_Token then
         return Null_Unbounded_Wide_Wide_String;
      end if;

      loop
         Append (Result, Text (Current_Token));
         exit when Current_Token = Token_End;
         Current_Token := Next (Current_Token);
      end loop;

      return Result;
   end Text;

   -----------------------
   -- Trailing_Comments --
   -----------------------

   function Trailing_Comments
     (Node : Ada_Node'Class)
      return Unbounded_Text_Type
   is
      Token_Start : Token_Reference;
      Token_End   : Token_Reference;

   begin
      Trailing_Comments (Node, Token_Start, Token_End);

      return Text (Token_Start, Token_End);
   end Trailing_Comments;

   -----------------------
   -- Trailing_Comments --
   -----------------------

   procedure Trailing_Comments
     (Node        : Ada_Node'Class;
      Token_Start : out Token_Reference;
      Token_End   : out Token_Reference)
   is
      Node_Token_End : constant Token_Reference := Node.Token_End;
      Current_Token  : Token_Reference          := Next (Node_Token_End);

      Comment_Block         : Token_Reference := No_Token;
      Comment_Seen          : Boolean         := False;
      Multi_Line_Break_Seen : Boolean         := False;

   begin
      Token_Start := No_Token;
      Token_End := No_Token;

      if Current_Token = No_Token then
         return;
      end if;

      Token_Start := Current_Token;

      loop
         exit when Current_Token = No_Token
                   or else Kind (Data (Current_Token))
                           not in Ada_Whitespace | Ada_Comment;

         Comment_Seen := @ or Kind (Data (Current_Token)) in Ada_Comment;

         if Multi_Line_Break_Seen then
            if Is_Multi_Line_Break (Current_Token) then
               Token_End := Comment_Block;
            elsif Kind (Data (Current_Token)) in Ada_Comment then
               Comment_Block := Current_Token;
            end if;

         else
            if Is_Multi_Line_Break (Current_Token) then
               Multi_Line_Break_Seen := True;
            elsif Kind (Data (Current_Token)) in Ada_Comment then
               Token_End := Current_Token;
            end if;
         end if;

         Current_Token := Next (Current_Token);
      end loop;

      if not Comment_Seen then
         Token_Start := No_Token;
         Token_End := No_Token;
         return;
      end if;
   end Trailing_Comments;

   -----------
   -- Visit --
   -----------

   overriding
   procedure Visit
     (Self : in out Prelude_Type; Clause : Use_Package_Clause_Type'Class)
   is
   begin
      Self.Leftover_Use_Section.Append (Use_Package_Clause_Type (Clause));
   end Visit;

   -----------
   -- Visit --
   -----------

   overriding
   procedure Visit
     (Self : in out Prelude_Type; Clause : With_Clause_Type'Class)
   is
      With_Clause : constant Libadalang.Analysis.With_Clause :=
        Clause.Node.As_With_Clause;

   begin
      if With_Clause.F_Has_Limited and With_Clause.F_Has_Private then
         Add_With_Clause (Self.Limited_Private_Section, Clause);

      elsif With_Clause.F_Has_Limited then
         Add_With_Clause (Self.Limited_Section, Clause);

      elsif With_Clause.F_Has_Private then
         Add_With_Clause (Self.Private_Section, Clause);

      else
         Add_With_Clause (Self.Public_Section, Clause);
      end if;
   end Visit;

end LAL_Refactor.Sort_Dependencies;
