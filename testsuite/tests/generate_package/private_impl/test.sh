#!/bin/sh

### Test generate new package body file
### For this test, we check multiple locations in the same line

# "package Priv_Impl is"
generate_package -P default.gpr -S priv_impl.ads -SL 1
# "private"
generate_package -P default.gpr -S priv_impl.ads -SL 8
# "end Priv_Impl;"
generate_package -P default.gpr -S priv_impl.ads -SL 15
# Negative tests
generate_package -P default.gpr -S priv_impl.ads -SL 4
generate_package -P default.gpr -S priv_impl.ads -SL 5

### Update existing package body (file in different directory)
# "package Priv_Impl.Child is"
generate_package -P default.gpr -S priv_impl-child.ads -SL 1
# "private"
generate_package -P default.gpr -S priv_impl-child.ads -SL 7
# "end Priv_Impl.Child;"
generate_package -P default.gpr -S priv_impl-child.ads -SL 9

# Negative tests
generate_package -P default.gpr -S priv_impl-child.ads -SL 2 -SC 5
generate_package -P default.gpr -S priv_impl-child.ads -SL 4 -SC 29
