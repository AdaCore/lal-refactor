procedure Main is
   function Square (X : Integer) return Natural is
   (X*X);

   procedure Increment --  blub
      (Y :     in out Natural);

   procedure Nested;

   Tracker : Natural := 40;

   procedure Increment (Y : in out Natural) is
   begin
      Tracker := @ + 1;
      Y := @ + 1;
   end Increment;

   procedure Nested
   is
      procedure Traverse --  Declaration with trivia
        (N : Integer;
         F : not null access function (NN : Integer) return String);

      function Int_Image (N : Integer'Class) return String;
      function Int_Image (N : Integer)       return String;

      function Add (L, R : Integer) return Integer;

      procedure Increment (X : in out Natural);

      procedure Traverse
        (N : Integer; F : not null access function (NN : Integer) return String) is
      begin
         while N > 0 loop
            Ada.Text_IO.Put_Line (F (N));
            N := N - 1;
         end loop;
      end Traverse;

      function Add (L,
      R : Integer)
       return Integer is
      begin
         return L + R;
      end Add;
   begin
      Traverse (NUP, Int_Image'Access);
   end Nested;
begin
   null;
end Main;