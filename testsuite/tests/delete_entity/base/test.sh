cp ./main.orig ./main.adb
delete_entity -P default.gpr -S main.adb -L 2 -R 14
echo "--  (main.orig) -L 2 -R 14"
cat ./main.adb

cp ./main2.orig ./main.adb
delete_entity -P default.gpr -S main.adb -L 2 -R 14
echo "--  (main2.orig) -L 2 -R 14"
cat ./main.adb

cp ./main3.orig ./main.adb
delete_entity -P default.gpr -S main.adb -L 9 -R 14
echo "--  (main3.orig) -L 9 -R 14"
cat ./main.adb

cp ./main4.orig ./main.adb
delete_entity -P default.gpr -S main.adb -L 2 -R 14
echo "--  (main4.orig) -L 2 -R 14"
cat ./main.adb

cp ./main5.orig ./main.adb
delete_entity -P default.gpr -S main.adb -L 3 -R 20
echo "--  (main5.orig) -L 3 -R 20"
cat ./main.adb

cp ./main6.orig ./main.adb
delete_entity -P default.gpr -S main.adb -L 2 -R 14
echo "--  (main6.orig) -L 2 -R 14"
cat ./main.adb

cp ./main7.orig ./main.adb
delete_entity -P default.gpr -S main.adb -L 8 -R 14
echo "--  (main7.orig) -L 8 -R 14"
cat ./main.adb

