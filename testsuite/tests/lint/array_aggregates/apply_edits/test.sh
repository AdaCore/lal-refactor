rm -rf output
mkdir output
cp test.gpr main.adb output
lalrefactor array_aggregates -P output/test.gpr
diff expected output --strip-trailing-cr
