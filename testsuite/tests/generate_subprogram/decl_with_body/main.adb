procedure Main is
   type Coordinates is array (Positive range 1..2) of Integer;

   function Square (X : Integer) return Natural is (X*X);

   procedure Increment --  blub
      (Y :     in out Natural);

   procedure Nested;

   function Taxicab_Distance (L, R : Coordinates) return Natural;

   --  Bodies ---

   procedure Increment (Y : in out Natural) is
   begin
      Tracker := @ + 1;
      Y := @ + 1;
   end Increment;

   procedure Nested
   is
      type T is tagged null record;
      T_Obj : constant T := null;

      procedure Traverse --  Declaration with trivia
        (Obj : T;
         F : not null access function (X : T) return String);

      function Int_Image (N : T'Class) return String;
      function Int_Image (N : T)       return String;

      function Add (L, R : Integer) return Integer;

      procedure Increment (X : in out Natural);

      --  Bodies ---

      procedure Traverse
        (Obj : T; F : not null access function (X : T) return String) is
      begin
         Ada.Text_IO.Put_Line (F (Obj));
      end Traverse;

      function Add (L,
      R : Integer)
       return Integer is
      begin
         return L + R;
      end Add;
   begin
      Traverse (7, Int_Image'Access);
   end Nested;
begin
   null;
end Main;