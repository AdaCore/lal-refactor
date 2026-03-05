#!/bin/sh

### Test generation of new package body file

## Test generation with only one public subprogram
generate_package -P default.gpr -S single_pub.ads -SL 1
## Test generation with only one private subprogram
generate_package -P default.gpr -S single_priv.ads -SL 1

# "package Mixed_Mode is"
generate_package -P default.gpr -S mixed_mode.ads -SL 1
# "private"
generate_package -P default.gpr -S mixed_mode.ads -SL 8
# "end Mixed_Mode;"
generate_package -P default.gpr -S mixed_mode.ads -SL 17

# Negative tests
generate_package -P default.gpr -S mixed_mode.ads -SL 4
generate_package -P default.gpr -S mixed_mode.ads -SL 5
