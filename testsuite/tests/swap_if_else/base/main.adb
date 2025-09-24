
with Ada.Text_IO;
with Interfaces.C; use Interfaces.C;

procedure Main is
   B : Boolean := False;
   L : Natural := 0;
begin
   if L = 0 then
      Ada.Text_IO.Put_Line ("if");
   elsif B then
      Ada.Text_IO.Put_Line ("elsif");
   else
      Ada.Text_IO.Put_Line ("else");
   end if;
   
   if L = 0 then
      Ada.Text_IO.Put_Line ("if");
   elsif B then
      Ada.Text_IO.Put_Line ("elsif1");
   elsif L > 5 then
      Ada.Text_IO.Put_Line ("elsif2");
   else
      Ada.Text_IO.Put_Line ("else");
   end if;
end Main;
