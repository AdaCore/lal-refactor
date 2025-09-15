cp ./main.orig ./main.adb
swap_if_not -P default.gpr -S main.adb -SL 11 -SC 8
echo "--  -SL 11 -SC 8"
cat ./main.adb

cp ./main.orig ./main.adb
swap_if_not -P default.gpr -S main.adb -SL 16 -SC 8
echo "--  -SL 16 -SC 8"
cat ./main.adb

cp ./main.orig ./main.adb
swap_if_not -P default.gpr -S main.adb -SL 25 -SC 8
echo "--  -SL 25 -SC 8"
cat ./main.adb

swap_if_not -P default.gpr -S main.adb -SL 25 -SC 8
echo "--  -SL 25 -SC 8"
cat ./main.adb

cp ./main.orig ./main.adb
swap_if_not -P default.gpr -S main.adb -SL 32 -SC 8
echo "--  -SL 32 -SC 8"
cat ./main.adb

cp ./main.orig ./main.adb
swap_if_not -P default.gpr -S main.adb -SL 39 -SC 8
echo "--  -SL 39 -SC 8"
cat ./main.adb
