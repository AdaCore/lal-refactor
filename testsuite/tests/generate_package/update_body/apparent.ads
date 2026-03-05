package Apparent is
   type Animal is tagged private;

   function "<" (L, R : Animal'Class) return Boolean;

   function Speak (A : Animal'Class) return String;

   function Native_Habitat (A : Animal'Class) return String;

   function Could_Beat_Human (A : Animal'Class) return Boolean;

   procedure Print (A : Animal'Class);
private
   type Creature is (Platypus, Nautilus, Walrus, Octopus);
   type Animal is tagged record
      Form : Creature;
      Age  : Natural;
   end record;

   function Could_Beat_Human (A : Animal'Class) return Boolean
   is (True);

   procedure Fossilize (A : Animal'Class);
end Apparent;