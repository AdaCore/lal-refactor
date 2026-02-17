#!/bin/sh

### Declarations split over multiple lines will be
### tested at different start column positions

#   function Main.Square (X : Integer) return Natural;
generate_subprogram -P default.gpr -S main.adb -SL 2 -SC 1
generate_subprogram -P default.gpr -S main.adb -SL 3 -SC 1

# Multi-line declaration
#   procedure Main.Increment
generate_subprogram -P default.gpr -S main.adb -SL 5 -SC 1
generate_subprogram -P default.gpr -S main.adb -SL 5 -SC 21
#      (Y : in out Natural);
generate_subprogram -P default.gpr -S main.adb -SL 6 -SC 1

#   procedure Main.Nested;
generate_subprogram -P default.gpr -S main.adb -SL 8 -SC 1

# Variable declaration
generate_subprogram -P default.gpr -S main.adb -SL 10 -SC 1

# Increment body has different subp spec text formatting
#   procedure Main.Increment (Y : in out Natural) is
generate_subprogram -P default.gpr -S main.adb -SL 12 -SC 1
generate_subprogram -P default.gpr -S main.adb -SL 13 -SC 8
generate_subprogram -P default.gpr -S main.adb -SL 16 -SC 3

#   procedure Main.Nested body
generate_subprogram -P default.gpr -S main.adb -SL 18 -SC 1
generate_subprogram -P default.gpr -S main.adb -SL 19 -SC 5
#   procedure Main.Nested.Traverse : has body
generate_subprogram -P default.gpr -S main.adb -SL 20 -SC 1
generate_subprogram -P default.gpr -S main.adb -SL 21 -SC 7
generate_subprogram -P default.gpr -S main.adb -SL 22 -SC 1
#   function Main.Nested.Add (L, R : Integer) return Integer;
generate_subprogram -P default.gpr -S main.adb -SL 27 -SC 1

##  Positive tests : we expect to generate stubs for these:
#   procedure Main.Nested.Node_Image (Node'Class)
generate_subprogram -P default.gpr -S main.adb -SL 24 -SC 1
#   procedure Main.Nested.Node_Image (Node)
generate_subprogram -P default.gpr -S main.adb -SL 25 -SC 1

# Same subp_spec but different scope
#   procedure Main.Nested.Increment (X : in out Natural);
generate_subprogram -P default.gpr -S main.adb -SL 29 -SC 1

