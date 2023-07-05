package body Test is
   function Increment (Value : Integer) return Integer is
   begin
      return Value + 1;
   end Increment;

   procedure Bar is
      Number : constant Integer :=
        Increment (1);
   begin
         null;
   end Bar;
end Test;
