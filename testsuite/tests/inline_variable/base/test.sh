cp ./main.orig ./main.adb
inline_variable -P default.gpr -S main.adb -SL 4 -SC 5
echo "--  -SL 4 -SC 5"
cat ./main.adb

cp ./main.orig ./main.adb
inline_variable -P default.gpr -S main.adb -SL 5 -SC 6
echo "--  -SL 5 -SC 6"
cat ./main.adb

cp ./main.orig ./main.adb
inline_variable -P default.gpr -S main.adb -SL 6 -SC 6
echo "--  -SL 6 -SC 6"
cat ./main.adb

cp ./main1.orig ./main.adb
inline_variable -P default.gpr -S main.adb -SL 4 -SC 5
echo "--  main1 -SL 4 -SC 5"
cat ./main.adb

cp ./main1.orig ./main.adb
inline_variable -P default.gpr -S main.adb -SL 4 -SC 11
echo "--  main1 -SL 4 -SC 11"
cat ./main.adb

cp ./main1.orig ./main.adb
inline_variable -P default.gpr -S main.adb -SL 4 -SC 17
echo "--  main1 -SL 4 -SC 17"
cat ./main.adb
