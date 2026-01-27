procedure Main is
   procedure Scan (T : Struct);
   overriding procedure Scan (T : Named_Struct);
begin
   null;
end Main;
