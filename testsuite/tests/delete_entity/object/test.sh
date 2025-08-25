cp ./main.orig ./main.adb
delete_entity -P default.gpr -S main.adb -L 2 -R 4
echo "--  (main.orig) -L 2 -R 4"
cat ./main.adb

cp ./main.orig ./main.adb
delete_entity -P default.gpr -S main.adb -L 2 -R 7
echo "--  (main.orig) -L 2 -R 7"
cat ./main.adb

cp ./main2.orig ./main.adb
delete_entity -P default.gpr -S main.adb -L 4 -R 7
echo "--  (main2.orig) -L 4 -R 7"
cat ./main.adb
