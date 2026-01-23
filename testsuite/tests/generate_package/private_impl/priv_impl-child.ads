package Priv_Impl.Child is
   function Wide_Char_Count (S : String) return WordCountT;

   procedure Print (Self : WordCountT);

   procedure H;
private
   function Log_Fmt (W: WordCountT) return String;
end Priv_Impl.Child;
