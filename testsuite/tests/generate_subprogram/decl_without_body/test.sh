#!/bin/sh

#   function Square (X : Integer) return Natural;
generate_subprogram -P default.gpr -S main.adb -SL 4 -SC 1
generate_subprogram -P default.gpr -S main.adb -SL 4 -SC 3
generate_subprogram -P default.gpr -S main.adb -SL 4 -SC 49

# Multi-line declaration
#   procedure Increment
generate_subprogram -P default.gpr -S main.adb -SL 6 -SC 1
generate_subprogram -P default.gpr -S main.adb -SL 6 -SC 4
generate_subprogram -P default.gpr -S main.adb -SL 6 -SC 20
generate_subprogram -P default.gpr -S main.adb -SL 6 -SC 21

#      (Y : in out Natural);
generate_subprogram -P default.gpr -S main.adb -SL 7 -SC 1
generate_subprogram -P default.gpr -S main.adb -SL 7 -SC 7
generate_subprogram -P default.gpr -S main.adb -SL 7 -SC 8
generate_subprogram -P default.gpr -S main.adb -SL 7 -SC 20
generate_subprogram -P default.gpr -S main.adb -SL 7 -SC 21

#   procedure Unimplemented;
generate_subprogram -P default.gpr -S main.adb -SL 9 -SC 1
generate_subprogram -P default.gpr -S main.adb -SL 9 -SC 4
generate_subprogram -P default.gpr -S main.adb -SL 9 -SC 32
generate_subprogram -P default.gpr -S main.adb -SL 9 -SC 33

# Should not suggest refactoring for the remaining tests:

# Blank lines
generate_subprogram -P default.gpr -S main.adb -SL 8 -SC 1
generate_subprogram -P default.gpr -S main.adb -SL 5 -SC 1

# Remaining subprogram body
generate_subprogram -P default.gpr -S main.adb -SL 10 -SC 1
generate_subprogram -P default.gpr -S main.adb -SL 11 -SC 1
generate_subprogram -P default.gpr -S main.adb -SL 11 -SC 4


