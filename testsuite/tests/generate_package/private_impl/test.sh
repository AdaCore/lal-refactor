#!/bin/sh

# Generate new package body
generate_package -P default.gpr -S priv_impl.ads -SL 1
generate_package -P default.gpr -S priv_impl.ads -SL 4
generate_package -P default.gpr -S priv_impl.ads -SL 5
generate_package -P default.gpr -S priv_impl.ads -SL 8
# Check entire end name line
generate_package -P default.gpr -S priv_impl.ads -SL 15 -SC 1
generate_package -P default.gpr -S priv_impl.ads -SL 15 -SC 3
generate_package -P default.gpr -S priv_impl.ads -SL 15 -SC 4
generate_package -P default.gpr -S priv_impl.ads -SL 15 -SC 6
generate_package -P default.gpr -S priv_impl.ads -SL 15 -SC 14

# Update existing package body (not even in same directory)
generate_package -P default.gpr -S priv_impl-child.ads -SL 1
generate_package -P default.gpr -S priv_impl-child.ads -SL 4
generate_package -P default.gpr -S priv_impl-child.ads -SL 7