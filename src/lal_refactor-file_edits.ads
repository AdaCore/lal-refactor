--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  This package constains utilities to apply text edits on files

with Ada.Containers.Indefinite_Ordered_Maps;

with VSS.Strings;

package LAL_Refactor.File_Edits is

   use type VSS.Strings.Virtual_String;

   package File_Name_To_Virtual_String_Maps is new
     Ada.Containers.Indefinite_Ordered_Maps
       (Key_Type     => LAL_Refactor.File_Name_Type,
        Element_Type => VSS.Strings.Virtual_String);

   subtype File_Name_To_Virtual_String_Map is
     File_Name_To_Virtual_String_Maps.Map;

   procedure Apply_Edits
     (Edits : LAL_Refactor.Text_Edit_Map);
   --  Apply Edits on disk. If an exception happens while trying to apply
   --  an edit to a file, that file is skipped and the procedure continues
   --  with the next one.

   function Apply_Edits
     (Edits : LAL_Refactor.Text_Edit_Map)
      return File_Name_To_Virtual_String_Map;
   --  Apply Edits on and return buffers with the edits applied. If an
   --  exception occurs while trying to create a buffer with the edits,
   --  the returned map will not contain that buffer.

end LAL_Refactor.File_Edits;
