#!/bin/sh

### Already implemented: no action
# Add
generate_subprogram -P default.gpr -S many_pub.ads -SL 4
# Print
generate_subprogram -P default.gpr -S many_pub.ads -SL 13

# Fold declared between Add and Print
generate_subprogram -P default.gpr -S many_pub.ads -SL 7
# Map privately declared
generate_subprogram -P default.gpr -S many_pub.ads -SL 17


### Test insertion into an empty package body
# (so package body decl exists but no decls inside)

# Map privately declared
generate_subprogram -P default.gpr -S empty_body.ads -SL 2

### Test insertion with multiple private subprograms and one public
#   procedure Unroll;
generate_subprogram -P default.gpr -S many_priv.ads -SL 2
# private
#   procedure A;
generate_subprogram -P default.gpr -S many_priv.ads -SL 4
#   function Concat (C : Character; S : String) return String;
generate_subprogram -P default.gpr -S many_priv.ads -SL 8
## Already implemented:
#   procedure B;
generate_subprogram -P default.gpr -S many_priv.ads -SL 6
