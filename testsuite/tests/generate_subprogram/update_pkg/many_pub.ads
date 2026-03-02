package Many_Pub is
   type Int_List is array (Positive range <>) of Integer;

   function Add (L, R : Int_List) return Int_List;
   --  Lists may be empty

   function Fold
     (Start : Integer;
      List  : Int_List;
      Fn    : not null access function (L, R : Integer) return Integer)
      return Integer;

   procedure Print (List : Int_List);
   --  Printer
   --  Longer comment
private
   function Map
     (List : Int_List;
      Fn   : not null access function (I : Integer) return Integer)
      return Int_List;
end Many_Pub;