
with Ada.Text_IO;

procedure Main is
begin
   declare
      procedure Print (Str : String) is
      begin
         if Str /= "" then
            Ada.Text_IO.Put_Line (Str);
         end if;
      end Print;
   begin
      Print ("Hello");
   end;
end Main;
