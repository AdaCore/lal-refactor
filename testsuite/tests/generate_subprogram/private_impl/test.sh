#!/bin/sh

#    function WC (S : String) return WordCountT;
generate_subprogram -P default.gpr -S priv_impl.ads -SL 4 -SC 1
generate_subprogram -P default.gpr -S priv_impl.ads -SL 4 -SC 4
generate_subprogram -P default.gpr -S priv_impl.ads -SL 4 -SC 46

#    function Total_Lines (W : WordCountT) return Natural;
generate_subprogram -P default.gpr -S priv_impl.ads -SL 5 -SC 1
generate_subprogram -P default.gpr -S priv_impl.ads -SL 5 -SC 4
generate_subprogram -P default.gpr -S priv_impl.ads -SL 5 -SC 56
