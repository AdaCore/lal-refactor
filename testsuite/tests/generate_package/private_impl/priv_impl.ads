package Priv_Impl is
   type WordCountT is private;

   function Run_Word_Count (S : String) return WordCountT;
   function Get_Line_Count (W : WordCountT) return Natural;

   procedure M;
private
   type WordCountT is record
      Words, Lines, Bytes : Natural;
   end record;

   function Get_Line_Count (W : WordCountT) return Natural
   is (W.Lines);

   procedure Debug (W : WordCountT);
end Priv_Impl;
