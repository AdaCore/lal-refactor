--
--  Copyright (C) 2021-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

with GNAT.Directory_Operations; use GNAT.Directory_Operations;

with Ada.Text_IO; use Ada.Text_IO;

package body LAL_Refactor is

   ------------------
   -- Log_Progress --
   ------------------

   procedure Log_Progress
     (Current  : Natural;
      Total    : String;
      Message  : String)
   is
      Total_Length       : constant Natural := Total'Length;
      Current_Image      : constant String :=
        Ada.Strings.Fixed.Tail
          (Ada.Strings.Fixed.Trim (Current'Image, Ada.Strings.Both),
           Total_Length,
           '0');
   begin
      Refactor_Trace.Trace
        ("[" & Current_Image & "/" & Total & "] " & Message);
   end Log_Progress;

   function Image (S : Source_Location_Range) return String;
   --  Return a Source_Location_Range as a string in a human readable format

   procedure Print (E : Text_Edit);
   --  Print an Edit in an human readable format to the standard output

   procedure Print (S : Text_Edit_Ordered_Set);
   --  Print an Edit_Ordered_Set in an human readable format to the standart
   --  output.

   --------------
   -- Contains --
   --------------

   function Contains
     (Edits     : LAL_Refactor.Text_Edit_Map;
      File_Name : LAL_Refactor.File_Name_Type;
      Edit      : LAL_Refactor.Text_Edit)
         return Boolean
   is (Edits.Contains (File_Name) and then Edits (File_Name).Contains (Edit));

   -----------
   -- Image --
   -----------

   function Image (S : Source_Location_Range) return String is
   begin
      return Trim (S.Start_Line'Image, Both)
        & ":"
        & Trim (S.Start_Column'Image, Both)
        & "-"
        & Trim (S.End_Line'Image, Both)
        & ":"
        & Trim (S.End_Column'Image, Both);
   end Image;

   -----------
   -- Merge --
   -----------

   procedure Merge
     (Source : in out Text_Edit_Map;
      Target : Text_Edit_Map)
   is
      Map_Cursor : Text_Edit_Ordered_Maps.Cursor := Target.First;

   begin
      while Text_Edit_Ordered_Maps.Has_Element (Map_Cursor) loop
         if Source.Contains (Text_Edit_Ordered_Maps.Key (Map_Cursor)) then
            declare
               Set_Cursor : Text_Edit_Ordered_Sets.Cursor :=
                 Target.Constant_Reference (Map_Cursor).First;

            begin
               while Text_Edit_Ordered_Sets.Has_Element (Set_Cursor) loop
                  Source.Reference (Text_Edit_Ordered_Maps.Key (Map_Cursor)).
                    Insert (Text_Edit_Ordered_Sets.Element (Set_Cursor));
                  Text_Edit_Ordered_Sets.Next (Set_Cursor);
               end loop;
            end;

         else
            Source.Insert
              (Text_Edit_Ordered_Maps.Key (Map_Cursor),
               Text_Edit_Ordered_Maps.Element (Map_Cursor));
         end if;

         Text_Edit_Ordered_Maps.Next (Map_Cursor);
      end loop;
   end Merge;

   -----------
   -- Print --
   -----------

   procedure Print (E : Text_Edit) is
   begin
      Ada.Text_IO.Put_Line (Image (E.Location) & " " & To_String (E.Text));
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print (S : Text_Edit_Ordered_Set) is
   begin
      for E of S loop
         Print (E);
      end loop;
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print (M : Text_Edit_Map) is
      use Text_Edit_Ordered_Maps;
      C : Cursor := M.First;

   begin
      while Has_Element (C) loop
         Ada.Text_IO.Put_Line (Base_Name (Key (C)));
         Print (Element (C));
         Next (C);
      end loop;

      New_Line;
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print (S : File_Creation_Ordered_Set) is
   begin
      for F of S loop
         Ada.Text_IO.Put_Line (Base_Name (To_String (F.Filepath)));
         Ada.Text_IO.Put_Line (To_String (F.Content));
      end loop;
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print (S : File_Deletion_Ordered_Set) is
   begin
      for F of S loop
         Ada.Text_IO.Put_Line (Base_Name (To_String (F)));
      end loop;
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print (S : File_Rename_Ordered_Set) is
   begin
      for F of S loop
         Ada.Text_IO.Put_Line (Base_Name (To_String (F.Filepath)));
         Ada.Text_IO.Put_Line (Base_Name (To_String (F.New_Name)));
      end loop;
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print (E : Refactoring_Edits) is
   begin
      Print (E.Text_Edits);
      Print (E.File_Creations);
      Print (E.File_Deletions);
      Print (E.File_Renames);
   end Print;

   -----------------
   -- Safe_Insert --
   -----------------

   procedure Safe_Insert
     (Edits : in out Text_Edit_Ordered_Set;
      Edit  : Text_Edit) is
   begin
      if not Edits.Contains (Edit) then
         Edits.Insert (Edit);
      end if;
   end Safe_Insert;

   -----------------
   -- Safe_Insert --
   -----------------

   procedure Safe_Insert
     (Edits    : in out Text_Edit_Map;
      File_Name : File_Name_Type;
      Edit      : Text_Edit)
   is
      Edits_Set : Text_Edit_Ordered_Set;

   begin
      if Edits.Contains (File_Name) then
         Safe_Insert (Edits.Reference (File_Name), Edit);

      else
         Edits_Set.Insert (Edit);
         Edits.Insert (File_Name, Edits_Set);
      end if;
   end Safe_Insert;

end LAL_Refactor;
