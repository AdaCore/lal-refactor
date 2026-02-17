generic 
   type Comparison is (<>);
   type T is private;
   with function Cmp (L, R : T) return Comparison; --  Do not generate this
package Operators is
   function ">"  (L, R : T) return Boolean;        --  Do generate this
end Operators;