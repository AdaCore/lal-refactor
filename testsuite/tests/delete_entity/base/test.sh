cp ./main.orig ./main.adb
delete_entity -P default.gpr -S main.adb -L 2 -R 14
echo "--  -L 2 -R 14"
cat ./main.adb
