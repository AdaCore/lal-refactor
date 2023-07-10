--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  JSON output tool

with LAL_Refactor.Tools.Record_Components_Tool;
with LAL_Refactor.Tools.Array_Aggregates_Tool;
with LAL_Refactor.Tools.Suppress_Dead_Params_Tool;
with LAL_Refactor.Tools.Scope_Declarations_Tool;
with LAL_Refactor.Tools.Relocate_Decls_Tool;
with VSS.Text_Streams;

package LAL_Refactor.Output is

   procedure JSON_Serialize
     (Edits_Info : LAL_Refactor.Tools.Array_Aggregates_Tool.Aggregate_Edits;
      Stream     : in out VSS.Text_Streams.Output_Text_Stream'Class);

   procedure JSON_Serialize
     (Edits_Info : LAL_Refactor.Tools.Record_Components_Tool.Delete_Infos;
      Stream     : in out VSS.Text_Streams.Output_Text_Stream'Class);

   procedure JSON_Serialize
     (Edits_Info : LAL_Refactor.Tools.Suppress_Dead_Params_Tool.Edit_Infos;
      Stream     : in out VSS.Text_Streams.Output_Text_Stream'Class);

   procedure JSON_Serialize
     (Edits_Info : LAL_Refactor.Tools.Scope_Declarations_Tool.Modify_Info;
      Stream     : in out VSS.Text_Streams.Output_Text_Stream'Class);

   procedure JSON_Serialize
     (Edits_Info : LAL_Refactor.Tools.Relocate_Decls_Tool.Modify_Info;
      Stream     : in out VSS.Text_Streams.Output_Text_Stream'Class);

end LAL_Refactor.Output;
