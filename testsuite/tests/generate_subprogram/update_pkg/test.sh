#!/bin/sh

# Already implemented: no action
generate_subprogram -P default.gpr -S spec.ads -SL 4 -SC 1
generate_subprogram -P default.gpr -S spec.ads -SL 7 -SC 1

# Privately declared!!
generate_subprogram -P default.gpr -S spec.ads -SL 11 -SC 1
