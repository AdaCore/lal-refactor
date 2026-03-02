#!/bin/sh

### All of these subprograms should be successfully generated
### As there are no subprogram bodies to use as context,
### they should all have the same insertion point at the end of the declarative part

#   function Square (X : Integer) return Natural;
generate_subprogram -P default.gpr -S main.adb -SL 2

# Multi-line declaration
#   procedure Increment
generate_subprogram -P default.gpr -S main.adb -SL 5
#      (Y : in out Natural);  --  over
generate_subprogram -P default.gpr -S main.adb -SL 6

#   procedure Unimplemented;
generate_subprogram -P default.gpr -S main.adb -SL 9

# Should not suggest refactoring for the remaining tests

# Remaining parent subprogram body
generate_subprogram -P default.gpr -S main.adb -SL 10 -SC 1
generate_subprogram -P default.gpr -S main.adb -SL 11 -SC 1
generate_subprogram -P default.gpr -S main.adb -SL 11 -SC 4
generate_subprogram -P default.gpr -S main.adb -SL 12 -SC 1


