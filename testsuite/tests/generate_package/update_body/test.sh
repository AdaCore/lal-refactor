#!/bin/sh

##  Update parent package

# "package Apparent is"
generate_package -P default.gpr -S apparent.ads -SL 1
# "private"
generate_package -P default.gpr -S apparent.ads -SL 13
# "end Apparent;"
generate_package -P default.gpr -S apparent.ads -SL 24
# Negative tests
generate_package -P default.gpr -S apparent.ads -SL 4
generate_package -P default.gpr -S apparent.ads -SL 5

##  Update child package, which is in a different directory

# "package Apparent.Child is"
generate_package -P default.gpr -S apparent-child.ads -SL 2
# "private"
generate_package -P default.gpr -S apparent-child.ads -SL 14
# "end Apparent.Child;"
generate_package -P default.gpr -S apparent-child.ads -SL 26

# Negative tests
generate_package -P default.gpr -S apparent-child.ads -SL 2 -SC 5
generate_package -P default.gpr -S apparent-child.ads -SL 4 -SC 29

##  Edge case: package body object exists without child declarations
generate_package -P default.gpr -S empty_body.ads -SL 1 -SC 5