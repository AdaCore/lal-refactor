--
--  Copyright (C) 2023, AdaCore
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

end LAL_Refactor.Utils;
