cp ./main.orig ./main.adb
delete_entity -P default.gpr -S main.adb -L 4 -R 17
echo "--  (main.orig) -L 4 -R 17"
cat ./main.adb

cp ./main.orig ./main.adb
delete_entity -P default.gpr -S main.adb -L 9 -R 28
echo "--  (main.orig) -L 9 -R 28"
cat ./main.adb
