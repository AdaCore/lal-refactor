#!/bin/sh

##  Test Generate Subprogram in New_Pkg_Body mode
#    function Run_Word_Count (S : String) return WordCountT;
generate_subprogram -P default.gpr -S priv_impl.ads -SL 4 -SC 1
##   Function privately implemented
#    function Get_Line_Count (W : WordCountT) return Natural;
generate_subprogram -P default.gpr -S priv_impl.ads -SL 5 -SC 1

##  Test Generate Subprogram in Add_To_Pkg mode
#    function Wide_Char_Count (S : String) return WordCountT;
generate_subprogram -P default.gpr -S priv_impl-child.ads -SL 2
## Already implemented, should not be available
#    procedure Print (Self : WordCountT);
generate_subprogram -P default.gpr -S priv_impl-child.ads -SL 4
#    procedure H;
generate_subprogram -P default.gpr -S priv_impl-child.ads -SL 6
#    function Log_Fmt (W: WordCountT) return String;
generate_subprogram -P default.gpr -S priv_impl-child.ads -SL 8