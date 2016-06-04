#!/bin/bash

# This script evaluates lightblue on JSeM.
#
# Usage:
# ./evaluate_jsem.sh <tag_1> <tag_2> ... <tag_n>
#
# Python3 and lxml library are required to run the extraction script ("extract_jsem_problems.py").
# Or, the set of extracted problems can be downloaded here:
# https://www.dropbox.com/sh/3cxtw7zx9b179aw/AABl-lBkZSdzg9TuinEbjedNa?dl=0
#
# To ignore some phenomena-type or inference-type, attach "-" in front of a tag.
#
# <tag> : Evaluate problems annotated with <tag>
# <-tag> : Do not evaluate problems annotated with <tag>
#
# Example:
# ./evaluate_jsem.sh "generalized quantifier" negation entailment -presupposition
#
# The upper/lower case distinction is ignored. E.g. "Plural" and "plural"
#
# To do:
#  - latex outputs for CCG derivations & SRs
#  - counting parsing time and proving time
#  - Fix a bug: Some tags start with "-". E.g. "-mo (conjunctive particle)"
#

# Set the names of a plain directory for entailment problems
plain_dir="jsem_plain"

# Create a plain directory and extract jsem problems.
# A list of all problems with tags ("jsem_problem_list") is created.
if [ ! -d ${plain_dir} ]; then
  echo "Extracting problems from JSeM XML file."
  mkdir -p ${plain_dir}
  # rm jsem_problems_list
  python extract_jsem_problems.py jsem.xml ${plain_dir}
fi

# Create a temporary list from "jsem_problem_list"
# To avoid matching, e.g., the "verb" tag with "adverb" or
# "implicative verb", add "+" to each tag so that "+verb" is called
cat jsem_problems_list | tr '\t' '+' > temp_jsem_problems_list

# Create a file containing a list of problems
# tagged with target phenomena and inference type
tags=""
  for i in `seq 1 ${#}`
  do
      if [ `echo ${1} | grep '+'` ]; then
          echo "Plus (+) is an illegal character. Exit."
          exit 1
      elif [ `echo ${1} | cut -c 1` = "-" ]; then
	  tag=`echo ${1} | cut -c 2-${#1}`
	  grep -i -v "+${tag}" temp_jsem_problems_list > temp2_jsem_problems_list
      else
	  grep -i "+${1}" temp_jsem_problems_list > temp2_jsem_problems_list
      fi
      cat temp2_jsem_problems_list > temp_jsem_problems_list
      tag_elimspace=`echo ${1} | awk '{gsub(" ", "-"); print $0;}'`
      tags="${tags}_${tag_elimspace}"
      shift
  done

cat temp_jsem_problems_list | tr '+' '\t' > temp2_jsem_problems_list
mv temp2_jsem_problems_list temp_jsem_problems_list

# Set the name of a result directory
results_dir="jsem_results${tags}"

# Creat a result directory
if [ ! -d ${results_dir} ]; then
  mkdir -p ${results_dir}
fi

# Remove the previous jsem.files
if [ -e ${plain_dir}/jsem.files ]; then
  rm ${plain_dir}/jsem.files
fi

# Create a list of file names
cat temp_jsem_problems_list | awk -F'\t' '{print $1}' > ${plain_dir}/jsem.files
rm temp_jsem_problems_list

# Compile a coq static library
coqc ../coqlib.v

# Count the total number of problems evaluated
total=`cat ${plain_dir}/jsem.files | wc -l`
count=1

# Parse and prove each problem
for f in `cat ${plain_dir}/jsem.files`; do
  # echo "--- Evaluating ${f}..."
  echo "Evaluating ${f}... ("${count}"/"${total}")"
  count=$((count + 1))
  cat ${plain_dir}/${f} > ${results_dir}/${f/.txt/}.results
  cat ${plain_dir}/${f} | .././DTStoProlog >> ${results_dir}/${f/.txt/}.results

results=`cat ${results_dir}/${f/.txt/}.results \
  | sed 's/-- Preterm ---------/##/g' \
  | sed 's/-- Prolog input ---------/##/g' \
  | sed 's/-- After resolving @ --------/##/g' \
  | sed 's/-- After elimSigma --------/##/g' \
  | sed 's/-- Coq formula --------/##/g' \
  | sed 's/-- Coq code --------/##/g' \
  | sed 's/-- Answer --------/##/g'`

coqformula=`echo $results | awk -F"##" '{print $6}' | awk '{print $1}'`
answer=`echo $results | awk -F"##" '{print $NF}'`

# Output a system answer
# Handling parse errors
if [ ! `echo "${answer}" | grep "yes\|no\|unknown"` ]; then
  echo "" > ${results_dir}/${f/.txt/}.answer
# Handling Prolog errors
elif [ ${coqformula} = "_end_of_file" ]; then
  echo 'error' > ${results_dir}/${f/.txt/}.answer
else
  echo ${answer} > ${results_dir}/${f/.txt/}.answer
fi
done

# preterm=`echo $results | awk -F"##" '{print $2;}'`
# prolog=`echo $results | awk -F"##" '{print $3;}'`
# resolved=`echo $results | awk -F"##" '{print $4;}'`
# normal=`echo $results | awk -F"##" '{print $5;}'`
# coqformula=`echo $results | awk -F"##" '{print $6;}'`
# signatureLine=`echo $results | awk -F"##" '{print $7;}'`
# signature=`echo $signatureLine | sed 's/\./\.#/g' | tr '#' '\n' | sed 's/^ //g'`
# sys_answer=`echo $results | awk -F"##" '{print $14;}'`

# g1=`echo $prolog | sed 's/_//g'`
# g2=`echo $resolved | sed 's/_//g'`
# g3=`echo $normal | sed 's/_//g'`
# swipl -s Prolog/prolog2latex.pl -g main -t halt --quiet -- "${g1}" "${g2}" "${g3}"
# platex test.tex > /dev/null 2>&1
# dvipdfmx test.dvi > /dev/null 2>&1
# mv test.pdf ${results_dir}/${f}.sr.pdf
# rm test.tex

# Creat a "main_jsem.html" file for the summary of results
echo "Summarizing."
echo "<!doctype html>
<html lang='en'>
<head>
  <meta charset='UTF-8'>
  <title>Evaluation results of JSeM</title>
  <style>
    body {
      font-size: 1.5em;
    }
  </style>
</head>
<body>
<table border='1'>
<tr>
  <td>JSeM problem</td>
  <td>gold answer</td>
  <td>system answer</td>
  <td>proving time</td>
</tr>" > $results_dir/main_jsem.html
total_observations=0
correct_recognitions=0
attempts=0
total_proving_time=0
red_color="rgb(255,0,0)"
green_color="rgb(0,255,0)"
white_color="rgb(255,255,255)"
gray_color="rgb(136,136,136)"
for system_filename in `ls -v ${results_dir}/jsem_*.answer`; do
  base_filename=${system_filename##*/} # this line obtains the filename, without the directory path.
  gold_filename=${plain_dir}/${base_filename}
  gold_answer=`cat $gold_filename`
  system_answer=`cat $system_filename`
  # time_filename=${results_dir}/${base_filename/.answer/.time}
  # proving_time=`cat $time_filename`
  # total_proving_time=`echo "$total_proving_time + $proving_time" | bc -l`
  total_number=$((total_number + 1))
  color=$white_color
# excluding "undef" problems
  if [ "${gold_answer}" == "undef" ]; then
    color=$gray_color
  else
    total_observations=$((total_observations + 1))
    if [ "$gold_answer" == "$system_answer" ]; then
      correct_recognitions=$((correct_recognitions + 1))
      color=$green_color
# # identifying "no system answer" with "unknown"
#     elif [ "$gold_answer" == "unknown" ] && [ "$system_answer" == "" ]; then
#       correct_recognitions=$((correct_recognitions + 1))
#       color=$green_color
    else
      color=$red_color
    fi
  fi
  echo '
<tr>
  <td><a style="background-color:'$color';" href="'${base_filename/.answer/.results}'">'${base_filename/.answer/}'</a></td>
  <td>'$gold_answer'</td>
  <td>'$system_answer'</td>
  <td>'$proving_time's</td>
</tr>' >> $results_dir/main_jsem.html
done
accuracy=`echo "scale=3; $correct_recognitions / $total_observations" | bc -l`
average_proving_time=`echo "scale=2; $total_proving_time / $total_number" | bc -l`
echo "
<h5>tags: "${tags}"</h5>
</body>
</html>
" >> $results_dir/main_jsem.html

# <h4><font color="red">Accuracy: "$accuracy" ("${correct_recognitions}"/"${total_observations}") </font></h4>
# <h4><font color="red">Average proving time: "${average_proving_time}" </font></h4>

# Calcuate the statistics (accuracy, recall and precision), and create a confusion matrix
./accuracy.sh ${results_dir}/main_jsem.html > ${results_dir}/score.txt

cat ${results_dir}/score.txt
