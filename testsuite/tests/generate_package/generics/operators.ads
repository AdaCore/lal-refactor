generic 
   type Comparison is (Less, Equal, Greater);
   type T is private;
   with function Cmp (L, R : T) return Comparison;
package Operators is
   function ">"  (L, R : T) return Boolean;
end Operators;