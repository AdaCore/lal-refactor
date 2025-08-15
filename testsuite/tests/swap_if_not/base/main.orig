
with Ada.Text_IO;
with Interfaces.C; use Interfaces.C;

procedure Main is
   B : Boolean := False;
   L : Natural := 0;
begin
   
   --  Should not propose, no else
   if not B then
      null;
   end if;

   --  Should not propose, has elsif
   if not B then
      null;
   elsif L = 0 then
      null;
   else
      null;
   end if;

   --  Complex statment, should be `not (statment)`
   if not B and then L = 0 then
      Ada.Text_IO.Put_Line ("not B");
   else
      Ada.Text_IO.Put_Line ("B");
   end if;
   
   --  Should propose
   if not B then
      Ada.Text_IO.Put_Line ("not B");
   else
      Ada.Text_IO.Put_Line ("B");
   end if;

   --  Should propose, simple NOT on the top level
   if not (B and then L = 0) then
      Ada.Text_IO.Put_Line ("not B");
   else
      Ada.Text_IO.Put_Line ("B");
   end if;
   
end Main;
