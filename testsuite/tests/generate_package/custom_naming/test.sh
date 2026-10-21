#!/bin/sh

### Test generation of new package body file

# "package Custom_Pkg is"
generate_package -P default.gpr -S custom_pkg.bloop -SL 1
# "private"
generate_package -P default.gpr -S custom_pkg.bloop -SL 8
# "end Custom_Pkg;"
generate_package -P default.gpr -S custom_pkg.bloop -SL 17

# Negative tests
generate_package -P default.gpr -S custom_pkg.bloop -SL 4
generate_package -P default.gpr -S custom_pkg.bloop -SL 5
