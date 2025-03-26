package Test is
   function Convert (A : String) return Float is (Float'Value (A));

   function Other_Convert (A : String) return Integer is (Integer'Value (A));

   A : Float := Convert ("3.3");
end Test;
