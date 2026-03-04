with Ada.Strings.Unbounded;
package Apparent.Child is
   type Pet is new Animal with private;

   function Has_Tentacles (A : Animal'Class) return Boolean;

   procedure Visit_Vet (P : Pet'Class);
   
   function Is_Pettable (P : Pet'Class) return Boolean;

   procedure Draw (P : Pet'Class);

   function Genus (A : Animal'Class) return String;
private
   type Pet is new Animal with record
      Name     : Ada.Strings.Unbounded.Unbounded_String;
      Cuteness : Positive;
      Rabies   : Boolean;
   end record;

   function MicroChip_ID (P : Pet'Class) return String;

   function Is_Pettable (P : Pet'Class) return Boolean is
   (not P.Rabies and then P.Cuteness < 100);

end Apparent.Child;
