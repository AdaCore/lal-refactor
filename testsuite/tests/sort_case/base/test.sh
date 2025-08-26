cp ./main.orig1 ./main.adb
sort_case -P default.gpr -S main.adb -PL 13 -PC 14
echo ""
echo "--  orig1 -PL 13 -PC 14"
cat ./main.adb

cp ./main.orig2 ./main.adb
sort_case -P default.gpr -S main.adb -PL 12 -PC 13
echo ""
echo "--  orig2 -PL 12 -PC 13"
cat ./main.adb

cp ./main.orig3 ./main.adb
sort_case -P default.gpr -S main.adb -PL 12 -PC 16
echo ""
echo "--  orig3 -PL 12 -PC 16"
cat ./main.adb

cp ./main.orig1 ./main.adb
sort_case -P default.gpr -S main.adb -PL 13 -PC 14 -OD declaration
echo ""
echo "--  orig1 -PL 13 -PC 14 declare"
cat ./main.adb

cp ./main.orig3 ./main.adb
sort_case -P default.gpr -S main.adb -PL 12 -PC 14 -OD declaration
echo ""
echo "--  orig3 -PL 12 -PC 14 declare"
cat ./main.adb

cp ./main.orig4 ./main.adb
sort_case -P default.gpr -S main.adb -PL 12 -PC 16 -OD declaration
echo ""
echo "--  orig4 -PL 12 -PC 16 declare"
cat ./main.adb

cp ./main.orig5 ./main.adb
sort_case -P default.gpr -S main.adb -PL 12 -PC 15 -OD declaration
echo ""
echo "--  orig5 -PL 12 -PC 15 declare"
cat ./main.adb
