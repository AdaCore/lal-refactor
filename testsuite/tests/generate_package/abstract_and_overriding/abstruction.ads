package Abstruction is
   type K is abstract tagged null record;
   procedure Hash (Key, Value : K'Class) is abstract;

   type V is new K with record
      X : Integer;
   end record;

   overriding procedure Hash (Key, Value : V'Class);
end Abstruction;
