#!/bin/bash

testsuite="fortest.pl"
outputCsv="./output/swi_alligator.csv"

function evaluate(){
  logic=$1
  number=$2

  count=0
  total=0

  if [ $number -gt 0 ]; then
    for id in $(seq 1 ${number}); do
        echo $id / $number
        output=$(swipl -s ${testsuite} -g "evalyes(${logic},${id})" -t halt --quiet)
        if [ "`echo $output | grep 'ERROR'`" ] || [ "$output" = "" ] ; then
          output=$(swipl -s ${testsuite} -g "evalno(${logic},${id})" -t halt --quiet)
          if [ "`echo $output | grep 'ERROR'`" ] || [ "$output" = "" ] ; then
            echo error
            prediction=UNKNOWN
            gold=UNKNOWN
            fname=$(echo $output | awk -F'&' '{print $5}')
          else
            echo no
            prediction=NO
# $(echo $output | awk -F'&' '{print $3}' | tr "[:lower:]" "[:upper:]")
            gold=$(echo $output | awk -F'&' '{print $4}' | tr "[:lower:]" "[:upper:]")
            fname=$(echo $output | awk -F'&' '{print $5}')
          fi
        else
          echo yes
          prediction=YES
# $(echo $output | awk -F'&' '{print $3}' | tr "[:lower:]" "[:upper:]")
          gold=$(echo $output | awk -F'&' '{print $4}' | tr "[:lower:]" "[:upper:]")
          fname=$(echo $output | awk -F'&' '{print $5}')
        fi
        if [ ${prediction} == ${gold} ]; then
          let count++
          let total++
        else
          let total++
        fi
        echo ${fname}$"\t"" "$"\t"${gold}$"\t"${prediction}$"\t" >> $outputCsv
        fname=""
        gold=""
        prediction=""
    done

    # accuracy=`echo "scale=3; $count / $total" | bc -l`
    # echo "Accuracy: "$count" / "$total" = "$accuracy
  else
    echo no $logic
  fi
}
echo file$"\t"assestment$"\t"status$"\t"plResult$"\t"note > $outputCsv
ntest=$(cat ${testsuite} | grep -v % | grep -c "checkTheorem(test")
evaluate test $ntest

nsyn=$(cat ${testsuite} | grep -v % | grep -c "checkTheorem(syn")
evaluate syn $nsyn

echo finish
