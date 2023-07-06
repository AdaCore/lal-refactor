--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  Package with the definition of all tools

package LAL_Refactor.Tools is

   type Tool is (Array_Aggregates);
   --  type Tool is
   --    (Array_Aggregates,
   --     Record_Components,
   --     Suppress_Dead_Params,
   --     Scope_Declarations,
   --     Relocate_Decls);

   function Convert (Arg : String) return Tool;
   --  Returns Tool'Value of Arg. Raises Parse_Tool_Exception is Arg is
   --  not a Tool.

   function Tool_List return String;
   --  Returns all literals of Tool as a lower case string, concatenated with
   --  LF.

   function Find_First_Tool_Index return Natural;
   --  Find the index of the first Tool in the arguments passed to the
   --  command line.

   Parse_Tool_Exception : exception;

end LAL_Refactor.Tools;
