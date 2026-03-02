package Many_Priv is
   procedure Unroll;
private
   procedure A;  --  Docstring

   procedure B (N : Natural);

   function Concat (C : Character; S : String) return String;
end Many_Priv;