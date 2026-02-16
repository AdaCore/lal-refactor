
###  Negative tests : no refactoring possible
## Subprograms declared in the generic formal part
#    with function Cmp (L, R : T) return Comparison;
generate_subprogram -P default.gpr -S operators.ads -SL 4 -SC 10

# Generic subprogram declaration
generate_subprogram -P default.gpr -S simple.ads -SL 5 -SC 1
# Formal generic decl
generate_subprogram -P default.gpr -S simple.ads -SL 7 -SC 10
# Actual generic subprogram
generate_subprogram -P default.gpr -S simple.ads -SL 8 -SC 10
# Expression function
generate_subprogram -P default.gpr -S simple.ads -SL 11
generate_subprogram -P default.gpr -S simple.ads -SL 12 -SC 4
# Abstract subprogram declarations
generate_subprogram -P default.gpr -S simple.ads -SL 20
generate_subprogram -P default.gpr -S simple.ads -SL 22

### Positive tests
## Subprograms declared in the generic internal package
generate_subprogram -P default.gpr -S operators.ads -SL 6 -SC 1
# Standard package subprogram
generate_subprogram -P default.gpr -S simple.ads -SL 3 -SC 1