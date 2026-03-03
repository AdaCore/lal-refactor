with Ada.Text_IO;

package body Priv_Impl.Child is
   procedure Print (Self : WordCountT) is
   begin
      Ada.Text_IO.Put_Line ("Lines: " & Self.Lines'Image);
      Ada.Text_IO.Put_Line ("Words: " & Self.Words'Image);
      Ada.Text_IO.Put_Line ("Bytes: " & Self.Bytes'Image);
   end Print;

   function Add (L, R : Integer) return Integer;
   --  docs

   X : constant String := "Name";  --  hewwo
   --  docstring
   --  more so

   --  blub

   --  hmm
--  other stuff here
end Priv_Impl.Child;
