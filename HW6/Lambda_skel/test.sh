make
ls ./testcase/*.l > ./testfiles.temp

echo "" > ./testresult.temp
while read line || [ -n "$line" ]
do
  ./run $line >> ./testresult.temp
done < testfiles.temp

make clean