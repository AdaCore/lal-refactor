# Extract Is_Positive as a function
extract_subprogram -P default.gpr -S my_package.adb -SL 8 -SC 8 -EL 14 -EC 13 -N Foo_Is_Positive

# Cannot extract if-stmt from No_Extracting_Me as function or procedure
extract_subprogram -P default.gpr -S my_package.adb -SL 20 -SC 8 -EL 26 -EC 13 -N Fail_No_Extracting_Me

# ... unless we include the final return stmt outside of the if-stmt
extract_subprogram -P default.gpr -S my_package.adb -SL 20 -SC 8 -EL 27 -EC 19 -N Oh_No_I_Have_Been_Extracted

# Extract Print_Positive IfStmt as a procedure
extract_subprogram -P default.gpr -S my_package.adb -SL 33 -SC 8 -EL 39 -EC 13 -N Foo_Print_Positive

# Extract Is_Even IfStmt as a function
extract_subprogram -P default.gpr -S my_package.adb -SL 47 -SC 8 -EL 52 -EC 15 -N Foo_Is_Even

# Cannot extract Only_Then_Branch IfStmt as a function
extract_subprogram -P default.gpr -S my_package.adb -SL 58 -SC 8 -EL 60 -EC 13 -N Fail_Only_Then_Branch

# ... unless we include the final return stmt
extract_subprogram -P default.gpr -S my_package.adb -SL 58 -SC 8 -EL 61 -EC 18 -N Foo_Only_Then_Branch

# Cannot extract Not_Zero IfStmt as a function
extract_subprogram -P default.gpr -S my_package.adb -SL 67 -SC 8 -EL 71 -EC 13 -N Fail_Is_Zero

# ... unless we include the final return
extract_subprogram -P default.gpr -S my_package.adb -SL 67 -SC 8 -EL 72 -EC 18 -N Foo_Is_Zero

# PrintIfExpr
# Extract variable assignment as procedure?
extract_subprogram -P default.gpr -S my_package.adb -SL 79 -SC  8 -EL 82 -EC 26 -N Foo_Print_IfExpr_Asgmt
# Extract IfExpr in parentheses?
extract_subprogram -P default.gpr -S my_package.adb -SL 80 -SC 10 -EL 82 -EC 25 -N Foo_Print_IfExpr_Paren
# Extract IfExpr WITHOUT parentheses?
extract_subprogram -P default.gpr -S my_package.adb -SL 80 -SC 11 -EL 82 -EC 24 -N Foo_Print_IfExpr_Expr

# Not_Zero
# Extract inner IfStmt works as this is a simple IfStmt
extract_subprogram -P default.gpr -S my_package.adb -SL 93 -SC 11 -EL 98 -EC 16 -N Extract_Inner
# Extract outer IfStmt : fails; this is a nested IfStmt and we don't currently check recursively
extract_subprogram -P default.gpr -S my_package.adb -SL 90 -SC 8 -EL 99 -EC 13 -N Extract_Recursively

# Loop with IfStmt and ExitStmts
# -- Pass entire loop : fails as it does not end in a return
extract_subprogram -P default.gpr -S my_package.adb -SL 106 -SC 7 -EL 115 -EC 15 -N Fail_Extract_Loop
# -- Pass entire IfStmt within loop: fails due to orphaned exit statement
extract_subprogram -P default.gpr -S my_package.adb -SL 107 -SC 10 -EL 114 -EC 16 -N Fail_Outer_If
# -- Pass an IfStmt with a forbidden return: should fail
extract_subprogram -P default.gpr -S my_package.adb -SL 108 -SC 14 -EL 110 -EC 19 -N Fail_Inner_If
# -- Finally, include the return stmt
extract_subprogram -P default.gpr -S my_package.adb -SL 106 -SC 7 -EL 116 -EC 21 -N Extract_Loop_With_Return

# Checking case stmts still work
extract_subprogram -P default.gpr -S my_package.adb -SL 121 -SC 7 -EL 128 -EC 15 -N Extract_Case_Proc
extract_subprogram -P default.gpr -S my_package.adb -SL 133 -SC 7 -EL 138 -EC 15 -N Extract_Case_Fn

# Should fail as it is neither a procedure nor a function
extract_subprogram -P default.gpr -S my_package.adb -SL 143 -SC 7 -EL 148 -EC 15 -N Fail_Case_Mixed_Returns
# Now we include the final return
extract_subprogram -P default.gpr -S my_package.adb -SL 143 -SC 7 -EL 149 -EC 16 -N Extract_Case_Mixed_Returns