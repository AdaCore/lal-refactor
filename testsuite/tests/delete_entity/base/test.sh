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

cp ./main8.orig ./main.adb
delete_entity -P default.gpr -S main.adb -L 3 -R 14
echo "--  (main8.orig) -L 3 -R 14"
cat ./main.adb

cp ./main9.orig ./main.adb
delete_entity -P default.gpr -S main.adb -L 2 -R 14
echo "--  (main9.orig) -L 2 -R 14"
cat ./main.adb

cp ./main_10.orig ./main.adb
delete_entity -P default.gpr -S main.adb -L 2 -R 14
echo "--  (main_10.orig) -L 2 -R 14"
cat ./main.adb

cp ./main_11.orig ./main.adb
delete_entity -P default.gpr -S main.adb -L 2 -R 4
echo "--  (main_11.orig) -L 2 -R 4"
cat ./main.adb

cp ./main_11.orig ./main.adb
delete_entity -P default.gpr -S main.adb -L 4 -R 7
echo "--  (main_11.orig) -L 4 -R 7"
cat ./main.adb

cp ./main_12.orig ./main.adb
delete_entity -P default.gpr -S main.adb -L 4 -R 13
echo "--  (main_12.orig) -L 4 -R 13"
cat ./main.adb

cp ./main_13.orig ./main.adb
delete_entity -P default.gpr -S main.adb -L 3 -R 17
echo "--  (main_13.orig) -L 3 -R 17"
cat ./main.adb

cp ./main_14.orig ./main.adb
delete_entity -P default.gpr -S main.adb -L 3 -R 4
echo "--  (main_14.orig) -L 3 -R 4"
cat ./main.adb

cp ./main_15.orig ./main.adb
delete_entity -P default.gpr -S main.adb -L 3 -R 6
echo "--  (main_15.orig) -L 3 -R 6"
cat ./main.adb

cp ./main_16.orig ./main.adb
delete_entity -P default.gpr -S main.adb -L 13 -R 4
echo "--  (main_16.orig) -L 13 -R 4"
cat ./main.adb
