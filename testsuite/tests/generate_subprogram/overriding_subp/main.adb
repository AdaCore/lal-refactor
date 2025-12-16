procedure Main is
   procedure Scan (T : Struct);
   overriding procedure Scan (T : Named_Struct);

   function Hash (Key : K; Value : V) return Struct_Hash is abstract;
begin
   null;
end Main;
