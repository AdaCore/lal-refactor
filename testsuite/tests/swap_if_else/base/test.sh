cp ./main.orig ./main.adb
swap_if_else -P default.gpr -S main.adb -SL 11 -SC 8
echo "--  -SL 11 -SC 8"
cat ./main.adb

cp ./main.orig ./main.adb
swap_if_else -P default.gpr -S main.adb -SL 16 -SC 8
echo "--  -SL 16 -SC 8"
cat ./main.adb

cp ./main.orig ./main.adb
swap_if_else -P default.gpr -S main.adb -SL 25 -SC 8
echo "--  -SL 25 -SC 8"
cat ./main.adb

swap_if_else -P default.gpr -S main.adb -SL 25 -SC 8
echo "--  -SL 25 -SC 8"
cat ./main.adb

cp ./main.orig ./main.adb
swap_if_else -P default.gpr -S main.adb -SL 32 -SC 8
echo "--  -SL 32 -SC 8"
cat ./main.adb

cp ./main.orig ./main.adb
swap_if_else -P default.gpr -S main.adb -SL 39 -SC 8
echo "--  -SL 39 -SC 8"
cat ./main.adb

cp ./main_relation_op.orig ./main.adb
swap_if_else -P default.gpr -S main.adb -SL 10 -SC 10
echo "-- relation_op -SL 10 -SC 10"
cat ./main.adb

cp ./main_relation_op.orig ./main.adb
swap_if_else -P default.gpr -S main.adb -SL 17 -SC 10
echo "-- relation_op -SL 17 -SC 10"
cat ./main.adb

cp ./main_relation_op.orig ./main.adb
swap_if_else -P default.gpr -S main.adb -SL 24 -SC 10
echo "-- relation_op -SL 24 -SC 10"
cat ./main.adb

cp ./main_relation_op.orig ./main.adb
swap_if_else -P default.gpr -S main.adb -SL 31 -SC 10
echo "-- relation_op -SL 31 -SC 10"
cat ./main.adb

cp ./main_relation_op.orig ./main.adb
swap_if_else -P default.gpr -S main.adb -SL 38 -SC 10
echo "-- relation_op -SL 38 -SC 10"
cat ./main.adb

cp ./main_relation_op.orig ./main.adb
swap_if_else -P default.gpr -S main.adb -SL 45 -SC 10
echo "-- relation_op -SL 45 -SC 10"
cat ./main.adb

cp ./main_elsif.orig ./main.adb
swap_if_else -P default.gpr -S main.adb -SL 12 -SC 13
echo "-- elsif -SL 12 -SC 13"
cat ./main.adb

cp ./main_elsif.orig ./main.adb
swap_if_else -P default.gpr -S main.adb -SL 22 -SC 19
echo "-- elsif -SL 22 -SC 19"
cat ./main.adb

cp ./main_elsif.orig ./main.adb
swap_if_else -P default.gpr -S main.adb -SL 18 -SC 14
echo "-- elsif -SL 18 -SC 14"
cat ./main.adb

swap_if_else -P default.gpr -S main.adb -SL 20 -SC 17
echo "-- elsif -SL 20 -SC 17"
cat ./main.adb
