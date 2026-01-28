procedure Main is
   --  This is basically the worst case for formatting

   function My_Subprogram (
      Multiple   : Integer;  --  side comment
      --  full comment
      Parameters : String;   --  words go here
      --  and also here
      Here       : Positive  --  more words
            --  also here
   ) return --  disrupted return statement
   --  further padding
   Boolean;
   --  Here is a little docstring

   overriding
   procedure --  is this even a token anymore?
   Traverse (S --  is THIS legal?
   : --  is this even legal?
         not    --  this is
   null --  very bad
               access  --  formatting
      --  let's add this
   String'Class) --  moar?
   ;

begin
   null;
end Main;
