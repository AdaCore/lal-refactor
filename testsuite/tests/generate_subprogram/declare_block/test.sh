#!/bin/sh

# Main declarative part; all should stub
generate_subprogram -P default.gpr -S main.adb -SL 2 -SC 1
generate_subprogram -P default.gpr -S main.adb -SL 3 -SC 1
generate_subprogram -P default.gpr -S main.adb -SL 4 -SC 1

#   Main body : [first declare block]
#   Should stub Square and Unimplemented
#   Main.[function Square]
generate_subprogram -P default.gpr -S main.adb -SL 8 -SC 1
#   Main.[function F]
generate_subprogram -P default.gpr -S main.adb -SL 9 -SC 1
#   Main.[procedure Unimplemented]
generate_subprogram -P default.gpr -S main.adb -SL 10 -SC 1

#       Main.F body declare block
#       Should stub Square (A, B) and Blip
#       declare keyword
generate_subprogram -P default.gpr -S main.adb -SL 14 -SC 1
#       Main.[function F [function Square (X)]]
generate_subprogram -P default.gpr -S main.adb -SL 15 -SC 1
#       Main.[function F [function Square (A, B)]]
generate_subprogram -P default.gpr -S main.adb -SL 16 -SC 1
#       Main.[function F [procedure Blip]]
generate_subprogram -P default.gpr -S main.adb -SL 17 -SC 1
#       Main.[function F [procedure Increment]]
generate_subprogram -P default.gpr -S main.adb -SL 18 -SC 1

#   Main body : {second declare block}
#   Should stub Square

#   Main.{function Square}
generate_subprogram -P default.gpr -S main.adb -SL 39 -SC 1