echo "Test Exercise 1"

cd ./K_skel
make > /dev/null
ls ./examples/*.k- > testfiles.temp

allpass=1
while read line || [ -n "$line" ]
do
  ./run $line > testresult.temp
  diff testresult.temp "${line%.*}.out" --brief > /dev/null
  if [ $? != 0 ]
  then
    echo "test for $line failed!"
    echo "expected:"
    cat "${line%.*}.out"
    echo "received:"
    cat testresult.temp
    allpass=0
    break
  fi
done < testfiles.temp

if [ $allpass = 1 ]
then
  echo "Exercise 1 ALL PASS!"
fi

cd ../


echo "Test Excercise 2"
allpass=1
TEST_TIME=100
echo "test $TEST_TIME random numbers."
for money in {$1..$TEST_TIME}
do
  TEST_MONEY=$RANDOM
  echo $TEST_MONEY | ./K_skel/run ./ex4_2.txt > my4_2result.temp
  echo $TEST_MONEY | ./K_skel/run ./ex4_2_ref.txt > ref4_2result.temp
  diff my4_2result.temp ref4_2result.temp --brief
  if [ $? != 0 ]
  then
    echo "test for $line failed!"
    echo "expected:"
    cat ref4_2result.temp
    echo "received:"
    cat my4_2result.temp
    allpass=0
    break
  fi
done

if [ $allpass = 1 ]
then
  echo "Exercise 2 ALL PASS!"
fi

cd ./K_skel
make clean > /dev/null
rm -f *.temp > /dev/null
cd ../
rm -f *.temp > /dev/null