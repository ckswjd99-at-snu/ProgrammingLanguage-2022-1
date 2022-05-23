make
ls ./examples/*.m > ./testfiles.temp
ls ./examples/kcm-simple/*.m >> ./testfiles.temp
ls ./examples/testcase/lastyear/*.m >> ./testfiles.temp
ls ./examples/testcase/lastyear2/*.m >> ./testfiles.temp

while read line || [ -n "$line" ]
do
  echo $line
  ./run $line
done < testfiles.temp

make clean
rm testfiles.temp
