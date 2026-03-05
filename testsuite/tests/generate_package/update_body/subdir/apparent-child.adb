package body Apparent.Child is

   function Has_Tentacles (A : Animal'Class) return Boolean
   is (A.Form in Octopus);
   --  docstring

   --  Footnote
end Apparent.Child;
--  End comment
