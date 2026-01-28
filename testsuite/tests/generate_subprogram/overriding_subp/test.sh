#!/bin/sh

#   procedure Scan (T : Struct);
generate_subprogram -P default.gpr -S main.adb -SL 2 -SC 1
#   overriding procedure Scan (T : Struct);
generate_subprogram -P default.gpr -S main.adb -SL 3 -SC 1
