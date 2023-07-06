--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Strings.Unbounded;

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

end LAL_Refactor.Utils;
