with Ada.Text_IO;
-- Test different forms of IfStmts and see which can be extracted
package body My_Package is
   --  This should be extracted as a function but was not recognised
   --  as the start and end nodes are the same
   function Is_Positive (I : Integer) return Boolean is
   begin
      if (I > 0) then
         return True;
      elsif I = 0 then
         return False;
      else
         return False;
      end if;
   end Is_Positive;

   --  IfStmt alone cannot be extracted as function or procedure; must include return stmt
   function No_Extracting_Me (I : Integer) return Boolean is
   begin
      if (I > 0) then
         return True;
      elsif I = 0 then
         Ada.Text_IO.Put_Line ("zerooooo");
      else
         return False;
      end if;
      return False;
   end No_Extracting_Me;

   --  Should be able to extract as procedure
   procedure Print_Positive (I : Integer) is
   begin
      if (I > 0) then
         Ada.Text_IO.Put_Line ("Positive!");
      elsif I = 0 then
         Ada.Text_IO.Put_Line ("Zero");
      else
         Ada.Text_IO.Put_Line ("!Negative");
      end if;
   end Print_Positive;

   --  This is easily extracted as a function
   --  as start node is IfStmt, end node is ReturnStmt
   function Is_Even (I : Integer) return Boolean is
      B : Boolean;
   begin
      if (I mod 2 = 0) then
         B := True;
      else
         B := False;
      end if;
      return B;
   end Is_Even;

   --  Cannot extract IfStmt with only one branch
   function Only_Then_Branch (I : Integer) return Boolean is
   begin
      if (I > 0) then
         return False;
      end if;
      return True;
   end Only_Then_Branch;

   --  Cannot extract IfStmt lacking an else branch
   function Is_Zero (I : Integer) return Boolean is
   begin
      if (I > 0) then
         return False;
      elsif (I < 0) then
         return False;
      end if;
      return True;
   end Is_Zero;

   --  Can we extract IfExprs as programs? or only as variables?
   procedure Print_IfExpr (I : Integer) is
      S : Integer := 0;
   begin
      S :=
         (if (I > 0)
         then 10
         else 42);
      Ada.Text_IO.Put_Line (S'Image);
   end Print_IfExpr;

   --  This is such a bad way to write an if statement why have I done this.
   function Not_Zero (I : Integer) return Boolean is
      S : Integer := 0;
   begin
      if S > 0 then
         return True;
      else
         if (S = 0)
         then
            return False;
         else
            return True;
         end if;
      end if;
   end Not_Zero;

   function Looping_If (B, C : Boolean) return Boolean
   is
      D : Boolean := B and C;
   begin
      loop
         if B then
            if C then
               exit;
            end if;
            return C;
         else
            return D;
         end if;
      end loop;
      return D and C;
   end Looping_If;

   procedure Case_Proc (X : T) is
   begin
      case X is
         when A =>
            F (1);
         when B =>
            F (10);
         when others =>
            null;
      end case;
   end Case_Proc;

   function Case_Fn (X : T) return Positive is
   begin
      case X is
         when A =>
            return 1;
         when others =>
            return 69;
      end case;
   end Case_Fn;

   function Case_Mixed_Returns (X : T) return Positive is
   begin
      case X is
         when A =>
            return 1;
         when others =>
            Print_Err ("I am not a return!");
      end case;
      return 12;
   end Case_Fn;
end My_Package;

