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
  echo "\e[1;90m\e[41mA\e[101mL\e[43mL\e[42m \e[46mP\e[44mA\e[45mS\e[41mS\e[101m!\e[0m"
fi

make clean > /dev/null
rm -f *.temp > /dev/null