procedure Main is
   function Square (X : Integer) return Natural;
   --  One-liner function with docstring

   procedure Increment  --  Declaration split
      (Y : in out Natural);  --  over
      --  two lines with some comments in the middle

   procedure Unimplemented;  --  More comment
begin
   null;
end Main;