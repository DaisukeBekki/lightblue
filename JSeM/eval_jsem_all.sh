#!/bin/bash

# This is a script to evaluate a parser on GQ, Plural, Adjective, Verb, and Attitude sections in JSeM
#
# ./eval_jsem_all.sh <directory-name>
#

results_dir=$1
# results_dir='jsem_all'

if [ -d ${results_dir} ]; then
  echo -e "The directory \"${results_dir}\" already exists"
  echo "Please rename or delete it"
  exit 1
fi
if [ ! -d ${results_dir} ]; then
  mkdir ${results_dir}
fi

speed () {
grep 'Average' ${results_dir}/$1/main_jsem.html | awk '{print $5}'
}
accuracy () {
grep 'Accuracy' ${results_dir}/$1/score.txt | awk '{print $2}'
}
recall () {
grep 'Recall' ${results_dir}/$1/score.txt | awk '{print $2}'
}
precision () {
grep 'Precision' ${results_dir}/$1/score.txt | awk '{print $2}'
}
gcorrect () {
grep 'Gold_correct_total' ${results_dir}/$1/score.txt | awk '{print $2}'
}
sysanswer () {
grep 'System_answer_total' ${results_dir}/$1/score.txt | awk '{print $2}'
}
syscorrect () {
grep 'System_correct_total' ${results_dir}/$1/score.txt | awk '{print $2}'
}
correct () {
grep 'Accuracy' ${results_dir}/$1/score.txt | sed 's/(/ /' | sed 's/)/ /' | awk '{print $3}' | awk -F'/' '{print $1}'
}
total () {
grep 'Accuracy' ${results_dir}/$1/score.txt | sed 's/(/ /' | sed 's/)/ /' | awk '{print $4}'
}

# total () {
# grep 'Accuracy' ${results_dir}/$1/score.txt | sed 's/(/ /' | sed 's/)/ /' | awk '{print $3}' | awk -F'/' '{print $2}'
# }

base_results () {
cat $1 | \
  sed 's/^$/#/g' | \
  tr '\n' ' ' | \
  tr '#' '\n' | \
  sed 's/</ /g' | \
  sed 's/>/ /g' | \
  grep href | \
  # awk '{print $6" "$10" "$13}' \
  awk '{print $6" "$10" "$15}' \
  > base_results.txt
}

### Evaluate each section ###
echo "------------GQ section"
./evaluate_jsem.sh "Generalized Quantifier"
mv jsem_results/Generalized-Quantifier ${results_dir}/gq
echo "GQ section: Done------------"

echo "------------Plural section"
./evaluate_jsem.sh "Plural" "-nominal anaphora"
mv jsem_results/Plural_-nominal-anaphora ${results_dir}/plural
echo "Plural section: Done------------"

echo "------------Adjective section"
./evaluate_jsem.sh "Adjective"
mv jsem_results/Adjective ${results_dir}/adjective
echo "Adjective section: Done------------"

echo "------------Verb section"
./evaluate_jsem.sh "Verb"
mv jsem_results/Verb ${results_dir}/verb
echo "Verb section: Done------------"

echo "------------Attitude section"
./evaluate_jsem.sh "Attitude"
mv jsem_results/Attitude ${results_dir}/attitude
echo "Verb section: Done------------"

### Calculate accuracy, etc. ###
echo "Calculating..."
base_results ${results_dir}/gq/main_jsem.html
mv base_results.txt ${results_dir}/all.results.list

base_results ${results_dir}/plural/main_jsem.html
cat base_results.txt >> ${results_dir}/all.results.list

base_results ${results_dir}/adjective/main_jsem.html
cat base_results.txt >> ${results_dir}/all.results.list

base_results ${results_dir}/verb/main_jsem.html
cat base_results.txt >> ${results_dir}/all.results.list

base_results ${results_dir}/attitude/main_jsem.html
cat base_results.txt >> ${results_dir}/all.results.list

rm base_results.txt

gq_speed=`speed 'gq'`
gq_accuracy=`accuracy 'gq'`
gq_recall=`recall 'gq'`
gq_precision=`precision 'gq'`
gq_gcorrect=`gcorrect 'gq'`
gq_sysanswer=`sysanswer 'gq'`
gq_syscorrect=`syscorrect 'gq'`
gq_correct=`correct 'gq'`
gq_total=`total 'gq'`

plural_speed=`speed 'plural'`
plural_accuracy=`accuracy 'plural'`
plural_recall=`recall 'plural'`
plural_precision=`precision 'plural'`
plural_gcorrect=`gcorrect 'plural'`
plural_sysanswer=`sysanswer 'plural'`
plural_syscorrect=`syscorrect 'plural'`
plural_correct=`correct 'plural'`
plural_total=`total 'plural'`

adjective_speed=`speed 'adjective'`
adjective_accuracy=`accuracy 'adjective'`
adjective_recall=`recall 'adjective'`
adjective_precision=`precision 'adjective'`
adjective_gcorrect=`gcorrect 'adjective'`
adjective_sysanswer=`sysanswer 'adjective'`
adjective_syscorrect=`syscorrect 'adjective'`
adjective_correct=`correct 'adjective'`
adjective_total=`total 'adjective'`

verb_speed=`speed 'verb'`
verb_accuracy=`accuracy 'verb'`
verb_recall=`recall 'verb'`
verb_precision=`precision 'verb'`
verb_gcorrect=`gcorrect 'verb'`
verb_sysanswer=`sysanswer 'verb'`
verb_syscorrect=`syscorrect 'verb'`
verb_correct=`correct 'verb'`
verb_total=`total 'verb'`

attitude_speed=`speed 'attitude'`
attitude_accuracy=`accuracy 'attitude'`
attitude_recall=`recall 'attitude'`
attitude_precision=`precision 'attitude'`
attitude_gcorrect=`gcorrect 'attitude'`
attitude_sysanswer=`sysanswer 'attitude'`
attitude_syscorrect=`syscorrect 'attitude'`
attitude_correct=`correct 'attitude'`
attitude_total=`total 'attitude'`

total_number=`echo "(${gq_total} + ${plural_total} + ${adjective_total} + ${verb_total} + ${attitude_total})" | bc`

total_accuracy=`echo "scale=4; (${gq_correct} + ${plural_correct} + ${adjective_correct} + ${verb_correct} + ${attitude_correct}) / ${total_number}" | bc -l`

total_recall=`echo "scale=4; (${gq_syscorrect} + ${plural_syscorrect} + ${adjective_syscorrect} + ${verb_syscorrect} + ${attitude_syscorrect}) / (${gq_gcorrect} + ${plural_gcorrect} + ${adjective_gcorrect} + ${verb_gcorrect} + ${attitude_gcorrect})" | bc -l`

total_precision=`echo "scale=4; (${gq_syscorrect} + ${plural_syscorrect} + ${adjective_syscorrect} + ${verb_syscorrect} + ${attitude_syscorrect}) / (${gq_sysanswer} + ${plural_sysanswer} + ${adjective_sysanswer} + ${verb_sysanswer} + ${attitude_sysanswer})" | bc -l`

total_avspeed=`echo "scale=2; ((${gq_speed} * ${gq_total}) + (${plural_speed} * ${plural_total}) + (${adjective_speed} * ${adjective_total}) + (${verb_speed} * ${verb_total}) + (${attitude_speed} * ${attitude_total})) / ${total_number}" | bc -l`


echo -e "-----------------------------------------------------------\n"\
"     Gold |   count| accuracy| recall| precision| av.speed| \n"\
"-----------------------------------------------------------\n"\
"       GQ |     ${gq_total}|    ${gq_accuracy}|  ${gq_recall}|     ${gq_precision}|     ${gq_speed}| \n"\
"   Plural |      ${plural_total}|    ${plural_accuracy}|  ${plural_recall}|     ${plural_precision}|     ${plural_speed}| \n"\
"Adjective |      ${adjective_total}|    ${adjective_accuracy}|  ${adjective_recall}|     ${adjective_precision}|     ${adjective_speed}| \n"\
"     Verb |      ${verb_total}|    ${verb_accuracy}|  ${verb_recall}|     ${verb_precision}|     ${verb_speed}|\n"\
" Attitude |      ${attitude_total}|    ${attitude_accuracy}|  ${attitude_recall}|     ${attitude_precision}|     ${attitude_speed}|\n"\
"    Total |     ${total_number}|    ${total_accuracy}|  ${total_recall}|     ${total_precision}|     ${total_avspeed}|\n"\
> ${results_dir}/all.results.table

# Create a confusion matrix
./create_conf_matrix.sh ${results_dir}/all.results.list >> ${results_dir}/all.results.table

