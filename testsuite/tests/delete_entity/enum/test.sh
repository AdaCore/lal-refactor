cp ./main.orig ./main.adb
delete_entity -P default.gpr -S main.adb -L 2 -R 21
echo "--  (main.orig) -L 2 -R 21"
cat ./main.adb

cp ./main2.orig ./main.adb
delete_entity -P default.gpr -S main.adb -L 2 -R 18
echo "--  (main2.orig) -L 2 -R 18"
cat ./main.adb
