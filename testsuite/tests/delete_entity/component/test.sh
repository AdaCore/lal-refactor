cp ./main.orig ./main.adb
delete_entity -P default.gpr -S main.adb -L 2 -R 26
echo "--  (main.orig) -L 2 -R 26"
cat ./main.adb

cp ./main2.orig ./main.adb
delete_entity -P default.gpr -S main.adb -L 2 -R 23
echo "--  (main2.orig) -L 2 -R 23"
cat ./main.adb

cp ./main3.orig ./main.adb
delete_entity -P default.gpr -S main.adb -L 3 -R 7 
echo "--  (main3.orig) -L 3 -R 7"
cat ./main.adb
