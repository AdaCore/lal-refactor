# These tests should report no subprogram extraction available
#   Start of IfStmt
extract_subprogram -P default.gpr -S my_package.adb -SL 5 -SC 8 -EL 5 -EC 9 -N Foo
#   Entire IfStmt
extract_subprogram -P default.gpr -S my_package.adb -SL 5 -SC 8 -EL 7 -EC 13 -N Foo
#   IfStmt plus NullStmt
extract_subprogram -P default.gpr -S my_package.adb -SL 5 -SC 8 -EL 8 -EC 11 -N Foo

# These should be successfully extracted
#   Only final ReturnStmt
extract_subprogram -P default.gpr -S my_package.adb -SL 9 -SC 8 -EL 9 -EC 19 -N Foo
#   NullStmt followed by final ReturnStmt
extract_subprogram -P default.gpr -S my_package.adb -SL 8 -SC 8 -EL 9 -EC 19 -N Foo