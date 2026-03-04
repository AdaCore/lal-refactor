package body Apparent is

   function Speak (A : Animal'Class) return String
   is
   (case A.Form is
      when Platypus => "It's a mammal!",
      when Nautilus => "I lived!",
      when Walrus => "Coo coo ka choo",
      when Octopus => "I can play piano");
end Apparent;
