with Ada.Text_IO;
procedure Main is

   procedure Sub_Proc is
      Number : Integer := 1;

      procedure Extracted;

      procedure Extracted is
      begin
         for J in 1 .. 10 loop
            Number := Number + 1;
         end loop;
      end Extracted;

   begin
      Extracted;
   end Sub_Proc;
begin
   null;
end Main;
