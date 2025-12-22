#!/bin/sh

# Generate new package body
generate_package -P default.gpr -S priv_impl.ads

# Update existing package body (not even in same directory)
generate_package -P default.gpr -S priv_impl-child.ads