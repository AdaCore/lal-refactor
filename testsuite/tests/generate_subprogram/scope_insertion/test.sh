#!/bin/sh

### Nested declarations among multiple

#   function Square (X : Integer) return Natural;
generate_subprogram -P default.gpr -S main.adb -SL 4

#   procedure Increment
generate_subprogram -P default.gpr -S main.adb -SL 9
#      (Y : in out Natural);  --  over
generate_subprogram -P default.gpr -S main.adb -SL 10

###  Nested declarations with only one contextual subprogram

#      function Implemented.F return Boolean;
generate_subprogram -P default.gpr -S main.adb -SL 23

#      procedure One_Decl_Above.Insert_Me_Under_Last;
generate_subprogram -P default.gpr -S main.adb -SL 41

#      procedure One_Decl_Below.Insert_Me_Above;
generate_subprogram -P default.gpr -S main.adb -SL 67

#   function Many_Decls_One_Subp.Concat;
generate_subprogram -P default.gpr -S main.adb -SL 89
#   function Many_Decls_One_Subp.declare.Difference;
generate_subprogram -P default.gpr -S main.adb -SL 96
#   function One_Subp_Many_Lines.Trim;
generate_subprogram -P default.gpr -S main.adb -SL 126
#   function Decl_After_Body.To_String (X : Integer) return String;
generate_subprogram -P default.gpr -S main.adb -SL 154
#   function Decl_After_Body.To_String (X : Float) return String;
generate_subprogram -P default.gpr -S main.adb -SL 160

### Should not suggest refactoring for the remaining tests

#   procedure One_Decl_Above;
generate_subprogram -P default.gpr -S main.adb -SL 2
#   procedure Implemented;
generate_subprogram -P default.gpr -S main.adb -SL 7
#   procedure One_Decl_Below;
generate_subprogram -P default.gpr -S main.adb -SL 13
#   procedure Implemented is
generate_subprogram -P default.gpr -S main.adb -SL 21
#   type One_Decl_Above.X;
generate_subprogram -P default.gpr -S main.adb -SL 35
#   procedure One_Decl_Above.Last;
generate_subprogram -P default.gpr -S main.adb -SL 38
#      return Positive;
generate_subprogram -P default.gpr -S main.adb -SL 39
#   function One_Decl_Below.Absolute;
generate_subprogram -P default.gpr -S main.adb -SL 69
