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

./run ./examples/mytest1.m
./run ./examples/mytest2.m

./run ./examples/mytest1.m << END
5
1000
END

./run ./examples/test10.m
./run ./examples/test11.m
./run ./examples/test12.m
./run ./examples/test13.m
./run ./examples/test14.m
./run ./examples/test15.m
./run ./examples/test16.m
./run ./examples/test17.m
./run ./examples/test18.m
./run ./examples/test19.m
./run ./examples/test20.m << END
2
END
./run ./examples/test21.m
./run ./examples/test22.m
./run ./examples/test23.m
./run ./examples/test24.m
./run ./examples/test25.m
./run ./examples/test26.m
./run ./examples/test27.m
./run ./examples/test28.m
./run ./examples/test29.m

make clean