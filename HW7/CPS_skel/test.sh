make
ls ./examples/*.m > ./testfiles.temp

echo "" > ./testresult.temp
while read line || [ -n "$line" ]
do
  ./run $line
done < testfiles.temp

make clean