#!/bin/bash

testsuite="fortest.pl"

function evaluate(){
  logic=$1
  number=$2

  count=0
  total=0

  for id in $(seq 1 ${number}); do
    output=$(swipl -s ${testsuite} -g "eval(${logic},${id})" -t halt --quiet)
    if [ "`echo $output | grep 'ERROR'`" ]; then
      prediction="error"
      gold="yes/no"
    else
      prediction=$(echo $output | awk -F',' '{print $3}')
      gold=$(echo $output | awk -F',' '{print $4}')
    fi
    if [ ${prediction} == ${gold} ]; then
      let count++
      let total++
    else
      let total++
    fi
    echo $output
  done

  accuracy=`echo "scale=3; $count / $total" | bc -l`
  echo "Accuracy: "$count" / "$total" = "$accuracy
}

nljt=$(cat fortest.pl | grep -v % | grep -c "checkTheorem(ljt")

evaluate fof $nljt
