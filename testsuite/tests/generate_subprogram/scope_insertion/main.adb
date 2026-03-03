procedure Main is
   procedure One_Decl_Above;

   function Square (X : Integer) return Natural;
   --  One-liner function with docstring

   procedure Implemented;  --  More comment

   procedure Increment  --  Declaration split
      (Y : in out Natural);  --  over
      --  two lines with some comments in the middle

   procedure One_Decl_Below;

   procedure Many_Decls_One_Subp;

   -----------------
   -- Implemented --
   -----------------

   procedure Implemented
   is
      function F return Boolean;
      --  Nested subprogram
   begin
      null;
   end Implemented;

   --------------------
   -- One_Decl_Above --
   --------------------

   procedure One_Decl_Above
   is
      type X is new Integer;
      --  Docstring here

      function Last (S : String)
      return Positive;

      procedure Insert_Me_Under_Last;

      ----------
      -- Last --
      ----------

      function Last (S : String) return Positive
      is
      begin
         return S'Last;
      end Last;
      --  Expect subprogram generation under here
   begin
      null;
   end One_Decl_Above;

   --------------------
   -- One_Decl_Below --
   --------------------

   procedure One_Decl_Below is
      type Tagged_Type is tagged record
         X : Integer;
      end record;
      --  Comment

      procedure Insert_Me_Above;  --  Comment

      function Absolute (T : Tagged_Type'Class) return Positive;

      --  End here
   --  Indented oddly --
--  Block docstring
--  Long comment up here
      function Absolute (T : Tagged_Type'Class) return Positive
      is (if T.X > 0 then T.X else T.X * (-1));
      --  More docstring
   begin
      null;
   end One_Decl_Below;

   -------------------------
   -- Many_Decls_One_Subp --
   -------------------------

   procedure Many_Decls_One_Subp
   is
      X : Integer;
      function Concat (L, R : String) return String;
      Y : Float;
      S : String;
   begin
      declare
         X : Integer;
         procedure Y;
         function Difference (L, R : String) return String;

         Y : Float;

         -------
         -- Y --
         -------

         procedure Y is
         begin
            null;
         end Y;
            --  Comment with awkward indentation
   --  Even worse indentation

         S : String;

      begin
         
      end;
   end Many_Decls_One_Subp;

   -------------------------
   -- One_Subp_Many_Lines --
   -------------------------

   procedure One_Subp_Many_Lines is
      --  Comments here


      function Trim (S : String) return String;  --  Comment
      --  Docstring

   --  Stray comments

   begin
      null;
   end One_Subp_Many_Lines;

   ---------------------
   -- Decl_After_Body --
   ---------------------

   procedure Decl_After_Body is  --  Stub insertion must be after decl
      function "+" (L, R : String) return String is (L & R);
      ---------
      -- Rev --
      ---------

      --  Docstring above as well as comment box
      procedure Rev (S : in out String) is
      begin
         null;
      end Rev;  --  End docstring here

      A : constant Float := 4.6;
      --  List of heterogeneous declarations

      function To_String (X : Integer) return String;
      --  Subprogram decl after body section

      B : constant Float := 4.6;
      --  List of heterogeneous declarations

      function To_String (X : Float) return String;
      --  Subprogram decl after body section

      C : constant String := "blub";

   begin
      null;
   end Decl_After_Body;  --  Docstring
   --  Docstring under

begin
   null;
end Main;
