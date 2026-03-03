procedure Main is
   function Square (X : Integer) return Natural;
   --  Docstring
   --  Long one
   --  So very many lines

   procedure Increment (Y : in out Natural) with Post => Increment'Result > Y;
   --  More docs

   procedure Unimplemented;
   --  Docstrings

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
            --  Docstring
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
      procedure Unimplemented;

      function Square (I : Integer) return Natural with Pre => I < 10;

      procedure Unimplemented is
      begin
         null;
      end Unimplemented;
      --  Docstring followed by blank lines



   begin
      null;
   end;
end Main;