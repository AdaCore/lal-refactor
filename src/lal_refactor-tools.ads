--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  Package with the definition of all tools

with Libadalang.Common; use Libadalang.Common;

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

   type Direction  is (Forward, Backward);
   type Token_Kinds is
     array (Positive range <>) of Token_Kind;

   function Lookup
     (Unit  : Analysis_Unit;
      Token : in out Token_Reference;
      Going : Direction;
      Skip  : Token_Kinds)
      return Ada_Node;
   --  Finds the next Ada_Node relative to Token. Going controls the lookup
   --  direction. If Token already belongs to an Ada_Node, that node is
   --  returned. Returns No_Ada_Node if no node is found or if
   --  Token = No_Token.

   function Lookup
     (Unit  : Analysis_Unit;
      Token : Token_Reference;
      Going : Direction)
      return Ada_Node;

   function Next_Non_Whitespace
     (Token : Token_Reference;
      Going : Direction)
      return Token_Reference;
   --  Finds the next non white Token_Reference relative to Token. Going
   --  controls the lookup direction. Returns No_Token if no whitespace
   --  if found or if Token = No_Token.

   function Find
     (Token : Token_Reference;
      Kind  : Token_Kind;
      Going : Direction)
      return Token_Reference;
   --  Finds the position of the Token. Returns No_Token if not found.

   function Find_Parent
     (Node   : Ada_Node;
      Filter : access function (Node : Ada_Node'Class) return Boolean)
      return Ada_Node;
   --  Goes over Node's parents until the parent is not null and Filter
   --  returns True and return this parent.

   Parse_Tool_Exception : exception;

end LAL_Refactor.Tools;
