make

./run ./examples/test1.m

./run ./examples/test2.m << END
1000
100
END

./run ./examples/test3.m
./run ./examples/test4.m << END
100
END

./run ./examples/test5.m
./run ./examples/test6.m
./run ./examples/test7.m
./run ./examples/test8.m
./run ./examples/test9.m


make clean