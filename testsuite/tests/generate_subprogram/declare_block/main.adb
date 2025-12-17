procedure Main is
   function Square (X : Integer) return Natural;
   procedure Increment (Y : in out Natural) with Post => Increment'Result > Y;
   procedure Unimplemented;

begin
   declare
      function Square (X : Integer) return Natural;
      function F (A : Integer) return Positive;
      procedure Unimplemented;

      function F (A : Integer) return Positive is
      begin
         declare
            function Square (X : Integer) return Natural;
            function Square (A, B : Integer) return Natural;
            procedure Blip (C : Positive);
            procedure Increment (Y : in out Natural);

            function Square (X : Integer) return Natural
            is (X * X);

            procedure Increment (Y : in out Natural) is
            begin
               Y := Y + 3;
            end Increment;
            A : Positive;
         begin
            A := Square (Square (3));
         end;
         return A;
      end F;
   begin
      null;
   end;

   declare
      --  this one can be generated, as a treat
      function Square (I : Integer) return Natural with Pre => I < 10;

      procedure Unimplemented is
      begin
         null;
      end Unimplemented;
   begin
      null;
   end;
end Main;