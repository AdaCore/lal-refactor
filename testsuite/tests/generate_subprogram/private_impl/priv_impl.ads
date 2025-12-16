package Priv_Impl is
   type WordCountT is private;

   function Run_Word_Count (S : String) return WordCountT;
   function Get_Line_Count (W : WordCountT) return Natural;

private
   type WordCountT is record
      Words, Lines, Bytes : Natural;
   end record;

   function Get_Line_Count (W : WordCountT) return Natural
   is (W.Lines);
end Priv_Impl;