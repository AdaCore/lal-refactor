--
--  Copyright (C) 2023, AdaCore
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

   function Expand_SLOC_To_Docstring
     (Node : Ada_Node'Class) return Source_Location_Range
   with Pre => not Node.Is_Null;
   --  Expand node end SLOC to include docstring underneathd
end LAL_Refactor.Utils;
