--
--  Copyright (C) 2023-2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Strings.Unbounded;

with Libadalang.Common;
with Libadalang.Analysis;

package LAL_Refactor.Utils is

   function Get_Project_Analysis_Units
     (Project_Filename : String)
      return Libadalang.Analysis.Analysis_Unit_Array;
   --  Gets all units of a project whose name is defined by Project_Filename.
   --  Project_Filename can either be a full path or a filename in the current
   --  directory.

   type Sources_List is array (Positive range <>) of
     Ada.Strings.Unbounded.Unbounded_String;

   function Get_Analysis_Units_From_Sources_List
     (Sources          : Sources_List;
      Project_Filename : String := "")
      return Libadalang.Analysis.Analysis_Unit_Array;
   --  Gets all units defined by Sources.
   --  If Project_Filename is defined, then uses it to create a unit provider.
   --  Project_Filename can either be a full path or a filename in the current
   --  directory.

   type Search_Direction_Type is (Forward, Backward);

   function Skip_Trivia
     (Token     : Libadalang.Common.Token_Reference;
      Direction : Search_Direction_Type)
      return Libadalang.Common.Token_Reference;
   --  If Token is trivia, returns the next or previous one that is not.
   --  Direction controls the search direction. Forward will return the next
   --  token and Backward will return the previous one.
   --  If Token is No_Token, returns No_Token.

   function Find_Comment_Box
     (Name : Defining_Name'Class) return Source_Location_Range;
   --  Find comment box before corresponding declaration. Return
   --  No_Source_Location_Range is not found.

   function Expand_Start_SLOC (N : Ada_Node'Class) return Source_Location
   with Pre => not N.Is_Null and then N.Sloc_Range.Start_Line > 1;
   --  If N has a title box above, return the start SLOC of this box.
   --  Otherwise look for comments beginning immediately above N
   --  without blank lines in between, and return the start SLOC
   --  of the earliest comment.

   function Expand_End_SLOC (N : Ada_Node'Class) return Source_Location
   with Pre => not N.Is_Null;
   --  Search for a docstring beginning directly after N,
   --  return the end SLOC of this comment.
   --  This includes a comment on the same line after N terminates,
   --  or one or multiple lines directly beneath N,
   --  as long as there are no blank lines in between.

   function Get_Contextual_Insertion_Point
     (Subp : Subp_Decl) return Source_Location
   with Pre => not (Subp.Is_Null or else Subp.P_Parent_Basic_Decl.Is_Null);
   --  Determine where to insert a generated subprogram body stub.
   --  If any other subprograms are declared and implemented in the same scope,
   --  try to match the declaration order. Otherwise insert at the end.
   --
   --  Note that declaration scope and insertion scope differ for packages.
   --  Only match the order of subprograms implemented in the package body.
end LAL_Refactor.Utils;
