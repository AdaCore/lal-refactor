cp ./main.orig ./main.adb
delete_entity -P default.gpr -S main.adb -L 2 -R 12
echo "--  (main.orig) -L 2 -R 12"
cat ./main.adb
