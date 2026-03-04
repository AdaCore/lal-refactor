--
--  Copyright (C) 2023-2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with GNATCOLL.VFS;
with GNATCOLL.Projects;

with Langkit_Support.Diagnostics;
with Langkit_Support.Text;

with Libadalang.Project_Provider;

with LAL_Refactor.Command_Line;

package body LAL_Refactor.Utils is

   ----------------------
   -- Find_Comment_Box --
   ----------------------

   function Find_Comment_Box
     (Name : Defining_Name'Class) return Source_Location_Range
   is
      use Libadalang.Common;

      procedure Skip_Dashes
        (Token : in out Token_Reference;
         Ok    : in out Boolean);

      procedure Skip_Name
        (Token : in out Token_Reference;
         Name  : Langkit_Support.Text.Text_Type;
         Ok    : in out Boolean);

      function Begin_Of_Line (Token : Token_Reference) return Source_Location
      is (Line   => Start_Sloc (Sloc_Range (Data (Token))).Line,
          Column => 1);

      -----------------
      -- Skip_Dashes --
      -----------------

      procedure Skip_Dashes
        (Token : in out Token_Reference;
         Ok    : in out Boolean) is
      begin
         while Token /= No_Token loop
            Token := Previous (Token);

            exit when Token = No_Token;

            case Kind (Data (Token)) is
               when Ada_Whitespace =>
                  null;  --  continue search
               when Ada_Comment =>
                  if (for all Char of Text (Token) => Char = '-') then
                     --  found coment with dash/dashes
                     exit;
                  else
                     Ok := False;
                     exit;
                  end if;
               when others =>
                  Ok := False;
                  exit;
            end case;
         end loop;

         Ok := Ok and Token /= No_Token;
      end Skip_Dashes;

      ---------------
      -- Skip_Name --
      ---------------

      procedure Skip_Name
        (Token : in out Token_Reference;
         Name  : Langkit_Support.Text.Text_Type;
         Ok    : in out Boolean) is
      begin
         while Token /= No_Token loop
            Token := Previous (Token);

            exit when Token = No_Token;

            case Kind (Data (Token)) is
               when Ada_Whitespace =>
                  null;  --  continue search
               when Ada_Comment =>
                  if Text (Token) = Name then
                     --  found coment with dash/dashes
                     exit;
                  else
                     Ok := False;
                     exit;
                  end if;
               when others =>
                  Ok := False;
                  exit;
            end case;
         end loop;

         Ok := Ok and Token /= No_Token;
      end Skip_Name;

      Result : Source_Location_Range := No_Source_Location_Range;
      Decl   : Basic_Decl;
      First  : Token_Reference;  --  First token of the Decl
      Token  : Token_Reference;  --  First token of the box
      Ok     : Boolean := True;
   begin
      Decl := Name.P_Basic_Decl;
      First := (if Decl.Is_Null then No_Token else Decl.Token_Start);
      Token := First;
      Skip_Dashes (Token, Ok);
      Skip_Name (Token, "-- " & Name.Text & " --", Ok);
      Skip_Dashes (Token, Ok);

      if Ok then
         Result := Make_Range
           (Start_Sloc => Begin_Of_Line (Token),
            End_Sloc   => Begin_Of_Line (First));
      end if;

      return Result;
   exception
      when Libadalang.Common.Property_Error =>
         return No_Source_Location_Range;
   end Find_Comment_Box;

   ------------------------------------------
   -- Get_Analysis_Units_From_Sources_List --
   ------------------------------------------

   function Get_Analysis_Units_From_Sources_List
     (Sources          : Sources_List;
      Project_Filename : String := "")
      return Libadalang.Analysis.Analysis_Unit_Array
   is

   begin
      if Sources'Length = 0 then
         return [];
      end if;

      if Project_Filename /= "" then
         declare
            use GNATCOLL.Projects;
            use GNATCOLL.VFS;
            use Libadalang.Project_Provider;

            Project_Environment  : Project_Environment_Access;
            Project_Tree         : constant Project_Tree_Access :=
              new GNATCOLL.Projects.Project_Tree;
            Project_Virtual_File : constant Virtual_File :=
              Create (+Project_Filename);

            Context       : Analysis_Context;
            Unit_Provider : Unit_Provider_Reference;

         begin
            Initialize (Project_Environment);
            --  TODO: Use procedures in GNATCOLL.Projects to set scenario
            --  variables (Change_Environment), set the target
            --  and runtime (Set_Target_And_Runtime), etc.
            Project_Tree.Load
              (Root_Project_Path => Project_Virtual_File,
               Env               => Project_Environment);

            Unit_Provider :=
              Create_Project_Unit_Provider
                (Tree => Project_Tree,
                 Env  => Project_Environment);
            Context := Create_Context (Unit_Provider => Unit_Provider);

            return Units : Analysis_Unit_Array (Sources'Range) do
               for J in Units'Range loop
                  declare
                     use Langkit_Support.Diagnostics;

                     Unit : constant Analysis_Unit :=
                       Context.Get_From_File (To_String (Sources (J)));

                  begin
                     if Unit.Has_Diagnostics then
                        Refactor_Trace.Trace
                          ("WARNING: Source "
                           & Unit.Get_Filename
                           & " has diagnostics");
                        for Diagnostic of Unit.Diagnostics loop
                           Refactor_Trace.Trace
                             (To_Pretty_String (Diagnostic));
                        end loop;

                     else
                        Units (J) := Unit;
                     end if;
                  end;
               end loop;
            end return;
         end;

      else
         declare
            Context : constant Analysis_Context := Create_Context;

         begin
            return Units : Analysis_Unit_Array (Sources'Range) do
               for J in Units'Range loop
                  declare
                     use Langkit_Support.Diagnostics;

                     Unit : constant Analysis_Unit :=
                       Context.Get_From_File (To_String (Sources (J)));

                  begin
                     if Unit.Has_Diagnostics then
                        Refactor_Trace.Trace
                          ("WARNING: Source "
                           & Unit.Get_Filename
                           & " has diagnostics");
                        for Diagnostic of Unit.Diagnostics loop
                           Refactor_Trace.Trace
                             (To_Pretty_String (Diagnostic));
                        end loop;

                     else
                        Units (J) := Unit;
                     end if;
                  end;
               end loop;
            end return;
         end;
      end if;
   end Get_Analysis_Units_From_Sources_List;

   --------------------------------
   -- Get_Project_Analysis_Units --
   --------------------------------

   function Get_Project_Analysis_Units
     (Project_Filename : String)
      return Libadalang.Analysis.Analysis_Unit_Array
   is
      use GNATCOLL.Projects;
      use GNATCOLL.VFS;
      use Libadalang.Project_Provider;

      Project_Environment  : Project_Environment_Access;
      Project_Tree         : constant Project_Tree_Access :=
        new GNATCOLL.Projects.Project_Tree;
      Project_Virtual_File : constant Virtual_File :=
        Create (+Project_Filename);

      Context       : Analysis_Context;
      Unit_Provider : Unit_Provider_Reference;
      Sources       : Filename_Vectors.Vector;

   begin
      Initialize (Project_Environment);
      for Scenario_Variable of
        LAL_Refactor.Command_Line.Scenario_Variables.Get
      loop
         declare
            Equal_Index : constant Natural :=
              Ada.Strings.Unbounded.Index (Scenario_Variable, "=");
            Name        : constant String :=
              Ada.Strings.Unbounded.Slice
                (Scenario_Variable, 1, Equal_Index - 1);
            Value        : constant String :=
              Ada.Strings.Unbounded.Slice
                (Scenario_Variable,
                 Equal_Index + 1,
                 Ada.Strings.Unbounded.Length (Scenario_Variable));
         begin
            Project_Environment.Change_Environment (Name, Value);
         end;
      end loop;
      Project_Tree.Load
        (Root_Project_Path => Project_Virtual_File,
         Env               => Project_Environment);

      Unit_Provider :=
        Create_Project_Unit_Provider
          (Tree => Project_Tree,
           Env  => Project_Environment);
      Context := Create_Context (Unit_Provider => Unit_Provider);

      Sources := Source_Files (Project_Tree.all);

      return Units : Analysis_Unit_Array
                       (Sources.First_Index .. Sources.Last_Index)
      do
         for J in Units'Range loop
            declare
               use Langkit_Support.Diagnostics;

               Unit : constant Analysis_Unit :=
                 Context.Get_From_File (To_String (Sources.Element (J)));

            begin
               if Unit.Has_Diagnostics then
                  Refactor_Trace.Trace
                    ("WARNING: Source "
                     & Unit.Get_Filename
                     & " has diagnostics");
                  for Diagnostic of Unit.Diagnostics loop
                     Refactor_Trace.Trace (To_Pretty_String (Diagnostic));
                  end loop;

               else
                  Units (J) := Unit;
               end if;
            end;
         end loop;

         Refactor_Trace.Trace ("Found" & Units'Length'Image & " units");
      end return;
   end Get_Project_Analysis_Units;

   -----------------
   -- Skip_Trivia --
   -----------------

   function Skip_Trivia
     (Token     : Libadalang.Common.Token_Reference;
      Direction : Search_Direction_Type)
      return Libadalang.Common.Token_Reference
   is
      use Libadalang.Common;

   begin
      if Token = No_Token then
         return No_Token;
      end if;

      if not Token.Is_Trivia then
         return Token;
      end if;

      case Direction is
         when Forward =>
            return Next (Token, Exclude_Trivia => True);
         when Backward =>
            return Previous (Token, Exclude_Trivia => True);
      end case;
   end Skip_Trivia;

   -----------------------
   -- Expand_Start_SLOC --
   -----------------------

   function Expand_Start_SLOC (N : Ada_Node'Class) return Source_Location is
      use Libadalang.Common;

      Scope_Start   : constant Source_Location :=
        (if N.Previous_Sibling.Is_Null
         then N.P_Declarative_Scope.Sloc_Range.Start_Sloc
         else N.Previous_Sibling.Sloc_Range.End_Sloc);
      Node_Start    : constant Source_Location := N.Sloc_Range.Start_Sloc;
      Search_Range  : constant Source_Location_Range :=
        Make_Range (Scope_Start, Node_Start);
      Comment_Box   : constant Source_Location_Range :=
        (if N.Kind in Ada_Basic_Decl
         then Find_Comment_Box (N.As_Basic_Decl.P_Defining_Name)
         else No_Source_Location_Range);
      Comment_Start : Source_Location := Node_Start;
      Point         : Source_Location :=
        (Line_Number'Pred (Node_Start.Line), Column_Number'Last);
      T             : Token_Reference := N.Unit.Lookup_Token (Point);
      --  Start at the end of each line
   begin
      if Comment_Box not in No_Source_Location_Range then
         return Comment_Box.Start_Sloc;
      end if;

      --  Each line begins and ends in a whitespace token,
      --  with a comment token in between if one is present.
      --  Start at the end of a line and iterate backwards.
      --  Check whitespace tokens only contain one linebreak,
      --  only moving up a line once a comment is found.
      --  Exit once a blank line is found,
      --  or the trivial range is exhausted
      while T.Is_Trivia and Compare (Search_Range, Point) in Inside loop
         if T.Data.Kind in Ada_Comment then
            Comment_Start := T.Data.Sloc_Range.Start_Sloc;
            Point.Line := Line_Number'Pred (Comment_Start.Line);
            T := N.Unit.Lookup_Token (Point);
         else
            T := T.Previous (Exclude_Trivia => False);
            exit when T.Data.Sloc_Range.Start_Line < Point.Line;
            --  Empty line found
         end if;
      end loop;
      return Comment_Start;
   end Expand_Start_SLOC;

   ---------------------
   -- Expand_End_SLOC --
   ---------------------

   function Expand_End_SLOC (N : Ada_Node'Class) return Source_Location is
      use Libadalang.Common;

      Node_End     : constant Source_Location := N.Sloc_Range.End_Sloc;
      Scope_End    : constant Source_Location :=
        (if N.Next_Sibling.Is_Null
         then N.P_Declarative_Scope.Sloc_Range.End_Sloc
         else N.Next_Sibling.Sloc_Range.Start_Sloc);
      Search_Range : constant Source_Location_Range :=
        Make_Range (Node_End, Scope_End);
      Comment_End  : Source_Location := Node_End;
      Point        : Source_Location := (Line_Number'Succ (Node_End.Line), 1);
      T            : Token_Reference :=
        N.Token_End.Next (Exclude_Trivia => False);
   begin

      --  Similar to Expand_Start_SLOC, except iteration begins
      --  immediately after N ends,
      --  to catch a comment directly after "end [Name]" nodes.
      while T.Is_Trivia and Compare (Search_Range, Point) in Inside loop
         if T.Data.Kind in Ada_Comment then
            Comment_End := T.Data.Sloc_Range.End_Sloc;
            Point.Line := Line_Number'Succ (Comment_End.Line);
            T := N.Unit.Lookup_Token (Point);
         else
            T := T.Next (Exclude_Trivia => False);
            exit when T.Data.Sloc_Range.Start_Line > Point.Line;
            --  Empty line found
         end if;
      end loop;
      return Comment_End;
   end Expand_End_SLOC;

   ----------------
   -- Line_Above --
   ----------------

   function Line_Above (Node : Ada_Node'Class) return Line_Number is
      use Libadalang.Common;
      --  Check for clashes with another node in the line above

      function Is_Whitespace (L : Line_Number) return Boolean;

      function Is_Whitespace (L : Line_Number) return Boolean is
         T : Token_Reference :=
           Node.Unit.Lookup_Token ((L, Column_Number (1)));
      begin
         while T not in No_Token loop
            exit when T.Data.Sloc_Range.Start_Line > L;  --  End of line
            if T.Data.Kind not in Ada_Whitespace then
               return False;
            end if;
            T := T.Next (Exclude_Trivia => False);
         end loop;
         return True;
      end Is_Whitespace;

      Line : Line_Number := Expand_Start_SLOC (Node).Line;

   begin
      if Line > Line_Number'First
        and then Is_Whitespace (Line_Number'Pred (Line))
      then
         Line := Line_Number'Pred (Line);
      end if;
      return Line;
   exception
      when others =>
         return Node.Sloc_Range.Start_Line;
   end Line_Above;

   ------------------------------------
   -- Get_Contextual_Insertion_Point --
   ------------------------------------

   function Get_Contextual_Insertion_Point
     (Subp : Subp_Decl) return Source_Location
   is
      use Libadalang.Common;

      function First_Decl (Dec_Part : Declarative_Part'Class) return Ada_Node
      is (if not (Dec_Part.Is_Null
                  or else Dec_Part.F_Decls.Is_Null
                  or else Dec_Part.F_Decls.Children_Count = 0)
          then Dec_Part.F_Decls.First_Child
          else No_Ada_Node);
      --  Return first declaration node if it exists, No_Ada_Node otherwise

      function Last_Decl (Dec_Part : Declarative_Part'Class) return Ada_Node
      is (if not (Dec_Part.Is_Null
                  or else Dec_Part.F_Decls.Is_Null
                  or else Dec_Part.F_Decls.Children_Count = 0)
          then Dec_Part.F_Decls.Last_Child
          else No_Ada_Node);
      --  Return last declaration node if it exists, No_Ada_Node otherwise

      function Find_Prev_Subp
        (Node  : Ada_Node;
         Match : not null access function (N : Ada_Node'Class) return Boolean)
         return Ada_Node;
      --  Find previous node for which Match is true, otherwise No_Ada_Node
      --  Note: only iterates within declarative scope of Node.

      function Find_Next_Subp
        (Node  : Ada_Node;
         Match : not null access function (N : Ada_Node'Class) return Boolean)
         return Ada_Node;
      --  Find next node for which Match is true, otherwise No_Ada_Node
      --  Note: only iterates within declarative scope of Node

      function Package_Insertion (Subp : Subp_Decl) return Source_Location
      with
        Pre =>
          Subp.P_Parent_Basic_Decl.Kind in Ada_Base_Package_Decl
          and then not Subp.P_Parent_Basic_Decl.P_Body_Part_For_Decl.Is_Null;
      --  Given top-level subprogram declared in package specification,
      --  check the package body for similar subprograms declared
      --  alongside Subp, and insert a Subp body in the same order

      function Nested_Insertion (Subp : Subp_Decl) return Source_Location;
      --  Given a nested subprogram Subp, check the declarative scope
      --  for subprogram bodies which have been declared alongside Subp
      --  and insert Subp in the appropriate point

      --------------------
      -- Find_Prev_Subp --
      --------------------

      function Find_Prev_Subp
        (Node  : Ada_Node;
         Match : not null access function (N : Ada_Node'Class) return Boolean)
         return Ada_Node
      is
         Result : Ada_Node := Node;
      begin
         while not Result.Is_Null loop
            exit when Match (Result);
            Result := Result.Previous_Sibling;
         end loop;
         return Result;
      end Find_Prev_Subp;

      --------------------
      -- Find_Next_Subp --
      --------------------

      function Find_Next_Subp
        (Node  : Ada_Node;
         Match : not null access function (N : Ada_Node'Class) return Boolean)
         return Ada_Node
      is
         Result : Ada_Node := Node;
      begin
         while not Result.Is_Null loop
            exit when Match (Result);
            Result := Result.Next_Sibling;
         end loop;
         return Result;
      end Find_Next_Subp;

      ---------------------------------
      -- Package_Insertion --
      ---------------------------------

      function Package_Insertion (Subp : Subp_Decl) return Source_Location is
         --  Insert Subp above or below other subprograms which have been
         --  declared in the same spec and implemented in the same package body
         --  Note this excludes privately implemented subprograms

         function Has_Impl (N : Ada_Node'Class) return Boolean
         is (N.Kind in Ada_Subp_Decl_Range
             and then not N.As_Subp_Decl.P_Body_Part.Is_Null
             and then
               N.As_Subp_Decl.P_Body_Part.P_Semantic_Parent.Kind
               in Ada_Package_Body_Range);
         --  True if a subprogram is declared in the same package spec as Subp
         --  and then implemented in the corresponding package body

         Spec           : constant Base_Package_Decl :=
           Subp.P_Parent_Basic_Decl.As_Base_Package_Decl;
         Scope          : constant Declarative_Part'Class :=
           Subp.P_Declarative_Scope;
         Spec_Body      : constant Package_Body := Spec.P_Body_Part;
         Last_Body_Decl : constant Ada_Node := Last_Decl (Spec_Body.F_Decls);
         Scope_End      : constant Source_Location :=
           Spec_Body.F_Decls.Sloc_Range.End_Sloc;
         --  Default location is just before "end [Package name]"

         Prev_Subp : Ada_Node :=
           Find_Prev_Subp (Subp.Previous_Sibling, Has_Impl'Access);
         Next_Subp : Ada_Node :=
           Find_Next_Subp (Subp.Next_Sibling, Has_Impl'Access);
         Point     : Source_Location := (0, Column_Number (1));

      begin
         if Scope.Kind in Ada_Public_Part_Range and Next_Subp.Is_Null then
            Next_Subp :=
              Find_Next_Subp
                (First_Decl (Spec.F_Private_Part), Has_Impl'Access);
         elsif Scope.Kind in Ada_Private_Part_Range and Prev_Subp.Is_Null then
            Prev_Subp :=
              Find_Prev_Subp (Last_Decl (Spec.F_Public_Part), Has_Impl'Access);
         end if;

         if not Prev_Subp.Is_Null then
            Point.Line := Line_Below (Prev_Subp.As_Subp_Decl.P_Body_Part);
         elsif not Next_Subp.Is_Null then
            Point.Line := Line_Above (Next_Subp.As_Subp_Decl.P_Body_Part);
         elsif not Last_Body_Decl.Is_Null then
            Point.Line := Line_Below (Last_Body_Decl);
         else
            --  Insert into empty package body
            --  Edge case: use entire SLOC in case empty body is one line
            Point := Scope_End;
         end if;
         return Point;
      end Package_Insertion;

      --------------------------------
      -- Nested_Insertion --
      --------------------------------

      function Nested_Insertion (Subp : Subp_Decl) return Source_Location is
         --  As the subprogram body for Subp must occur after its declaration,
         --  any subprogram used as insertion context must occur after Subp.
         --  First try to insert under a subprogram body declared above,
         --  then above a subprogram body declared below,
         --  finally insert under the last declaration (which must exist)

         function Has_Impl (N : Ada_Node'Class) return Boolean
         is (N.Kind in Ada_Subp_Decl_Range
             and then not N.As_Subp_Decl.P_Body_Part.Is_Null
             and then
               Subp.Sloc_Range.End_Line
               < N.As_Subp_Decl.P_Body_Part.Sloc_Range.Start_Line);

         Scope     : constant Declarative_Part'Class :=
           Subp.P_Declarative_Scope;
         Last_Node : constant Ada_Node := Last_Decl (Scope);
         Prev_Subp : constant Ada_Node :=
           Find_Prev_Subp (Subp.Previous_Sibling, Has_Impl'Access);
         Next_Subp : constant Ada_Node :=
           Find_Next_Subp (Subp.Next_Sibling, Has_Impl'Access);
         Point     : Source_Location := (0, Column_Number (1));

      begin
         if not Prev_Subp.Is_Null then
            Point.Line := Line_Below (Prev_Subp.As_Subp_Decl.P_Body_Part);
         elsif not Next_Subp.Is_Null then
            Point.Line := Line_Above (Next_Subp.As_Subp_Decl.P_Body_Part);
         else
            Point.Line := Line_Below (Last_Node);
         end if;
         return Point;
      end Nested_Insertion;

      --  Default insertion at start of line
   begin
      return
        (case Subp.P_Parent_Basic_Decl.Kind is
           when Ada_Base_Package_Decl => Package_Insertion (Subp),
           when Ada_Subp_Body_Range   => Nested_Insertion (Subp),
           when others                =>
             raise Constraint_Error with Subp.P_Parent_Basic_Decl.Kind_Name);
   exception
      when E : others =>
         Refactor_Trace.Trace
           (E,
            "Failed to detect insertion point for "
            & Langkit_Support.Text.To_UTF8 (Subp.P_Defining_Name.Text));
         return No_Source_Location;
   end Get_Contextual_Insertion_Point;
end LAL_Refactor.Utils;
