package body Test is
   ---------
   -- Foo --
   ---------

   procedure Foo is
      procedure Bar;
      --  Bar

      ---------
      -- Bar --
      ---------

      procedure Bar is
      begin
         null;
      end Bar;

   begin
      null;
   end Foo;
end Test;
