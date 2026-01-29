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

   --  Comment at start!
   function Add (L, R : Integer)
   --  Comment sandwich
   return Integer;
   --  This is a docstring attached to function
   --  You can tell because there is only ony newline after
   --  And then every line is a comment.
   --
   --  Even that "blank" line is part of the comment!
         --  also this.

   --  Not meeeee tho
begin
   null;
end Main;
