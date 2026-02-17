package Simple is
   --  Subprogram declarations which can be generated
   procedure Standard (X : String'Class);

   generic
      type T is private;
      with function To_String (Item : T) return String;
   procedure Log (Item : T; Filename : String);

   --  All remaining subprogram declarations ineligible for Generate Subprogram
   function Expr_Fn (L, R : Integer'Class) return Integer
   is (L + R);

   type X is limited private;
   type Property is abstract tagged record
      Set : Boolean;
      Value : X;
   end record;

   procedure Set (P : Property; Value : X) is abstract;

   function Get (P : Property) return X is abstract;
private
   type X is (Cleared, Error, Address);
end Simple;