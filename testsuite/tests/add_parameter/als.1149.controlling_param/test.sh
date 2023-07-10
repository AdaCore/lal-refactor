#!/bin/sh

# Add a new first parameter to a primitive subprogram: it should not be added
add_parameter -P default.gpr -S main_package.ads -L 5 -R  18 \
    -N 'A : Integer'
