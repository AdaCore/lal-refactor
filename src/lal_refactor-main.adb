--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  LAL_Refactor tools driver

with Ada.Text_IO;

with GNATCOLL.Traces;

with LAL_Refactor.Command_Line;
with LAL_Refactor.Tools;
with LAL_Refactor.Tools.Array_Aggregates_Tool;

procedure LAL_Refactor.Main is
begin
   GNATCOLL.Traces.Parse_Config_File;

   if LAL_Refactor.Command_Line.Parser.Parse then
      if LAL_Refactor.Command_Line.Help.Get then
         Ada.Text_IO.Put_Line (LAL_Refactor.Command_Line.Parser.Help);

      else
         if LAL_Refactor.Command_Line.Verbose.Get then
            Refactor_Trace.Set_Active (True);
         end if;

         case LAL_Refactor.Command_Line.Tool.Get is
            when LAL_Refactor.Tools.Array_Aggregates =>
               LAL_Refactor.Tools.Array_Aggregates_Tool.Run;
         end case;
      end if;
   end if;
end LAL_Refactor.Main;
