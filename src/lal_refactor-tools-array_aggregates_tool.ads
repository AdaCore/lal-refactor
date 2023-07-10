--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  LAL_Refactor array aggregates tool

with Ada.Containers.Ordered_Maps;

with Libadalang.Analysis;

package LAL_Refactor.Tools.Array_Aggregates_Tool is

   function "<" (L, R : Libadalang.Analysis.Aggregate) return Boolean;
   --  First compares the Aggregate Analysis_Unit filename and then their
   --  Source_Location.

   package Aggregates_To_Text_Edit_Ordered_Set_Maps is new
     Ada.Containers.Ordered_Maps
       (Key_Type     => Libadalang.Analysis.Aggregate,
        Element_Type => LAL_Refactor.Text_Edit_Ordered_Set,
        "<"          => "<",
        "="          => LAL_Refactor.Text_Edit_Ordered_Sets."=");

   subtype Aggregate_Edits is Aggregates_To_Text_Edit_Ordered_Set_Maps.Map;

   function Upgrade_Array_Aggregates
     (Units : Libadalang.Analysis.Analysis_Unit_Array)
      return Aggregate_Edits;
   --  Runs the array aggregates tool on Units. This is the main entry point
   --  of this tool when used in library mode.

   function Upgrade_Array_Aggregates
     (Units : Libadalang.Analysis.Analysis_Unit_Array)
      return LAL_Refactor.Text_Edit_Map;
   --  Runs the array aggregates tool on Units. This is the main entry point
   --  of this tool when used in library mode.

   procedure Run;
   --  Runs the array aggregates tool.
   --  This procedure should by a driver only. User should use the library mode
   --  equivalent Upgrade_Array_Aggregates defined above.

end LAL_Refactor.Tools.Array_Aggregates_Tool;
