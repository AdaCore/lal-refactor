package body Many_Priv is
   --  Docstring up here
   --  Unrelated to any subprogram

   -------
   -- B --
   -------

   procedure B (N : Natural) is
   begin
      for I in 1 .. N loop
         null;
      end loop;
   end B;  --  Docstring
   --  continues all the way down here
   --  and then some

   --  Random comment down here
end Many_Priv;
