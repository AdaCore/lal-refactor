#!/bin/sh

# procedure Main is
### Negative tests : subprograms which are already implemented and other decls

#   type Coordinates is array (Positive range 1..2) of Integer;
generate_subprogram -P default.gpr -S main.adb -SL 2 -SC 1
#   function Square (X : Integer) return Natural;
generate_subprogram -P default.gpr -S main.adb -SL 4 -SC 1

# Multi-line declaration
#   procedure Increment
generate_subprogram -P default.gpr -S main.adb -SL 6 -SC 1
#      (Y : in out Natural);
generate_subprogram -P default.gpr -S main.adb -SL 7 -SC 21

#   procedure Nested;
generate_subprogram -P default.gpr -S main.adb -SL 9 -SC 1

# Increment body (formatted differently to declaration)
#   procedure Increment (Y : in out Natural) is
generate_subprogram -P default.gpr -S main.adb -SL 15 -SC 1
generate_subprogram -P default.gpr -S main.adb -SL 16 -SC 8
generate_subprogram -P default.gpr -S main.adb -SL 19 -SC 3

#   procedure Nested is
generate_subprogram -P default.gpr -S main.adb -SL 21 -SC 1

generate_subprogram -P default.gpr -S main.adb -SL 22 -SC 5
#      type T is tagged null record
generate_subprogram -P default.gpr -S main.adb -SL 23 -SC 5
#      T_Obj : constant T := null;
generate_subprogram -P default.gpr -S main.adb -SL 24 -SC 5

#      procedure Traverse --  Declaration with trivia
generate_subprogram -P default.gpr -S main.adb -SL 26 -SC 1
#        (Obj : T;
generate_subprogram -P default.gpr -S main.adb -SL 27 -SC 7
#         F : not null access function (X : T) return String);
generate_subprogram -P default.gpr -S main.adb -SL 28 -SC 1
#      function Add (L, R : Integer) return Integer;
generate_subprogram -P default.gpr -S main.adb -SL 33 -SC 1

###  Positive tests : should generate subprogram stubs for all these:

#   function Taxicab_Distance (L, R : Coordinates) return Natural;
generate_subprogram -P default.gpr -S main.adb -SL 11 -SC 1
#      procedure Int_Image (T'Class)
generate_subprogram -P default.gpr -S main.adb -SL 30 -SC 1
#      procedure Int_Image (T)
generate_subprogram -P default.gpr -S main.adb -SL 31 -SC 1
#      procedure Increment (X : in out Natural);
generate_subprogram -P default.gpr -S main.adb -SL 35 -SC 1

# end Main