#!/bin/bash

# This is a script to evaluate a parser on all sections in JSeM
#
# ./eval_jsem_all.sh <directory-name>
#
# all the results files are located in <directory-name>

results_dir=$1
# results_dir='jsem_all'

# Check if the directory for each section exists
if [ -d jsem_results/Generalized-Quantifier ]; then
  echo -e "The directory \"jsem_results/Generalized-Quantifier\" already exists"
  echo "Please rename or delete it"
  exit 1
fi
if [ -d jsem_results/Plural_-nominal-anaphora ]; then
  echo -e "The directory \"jsem_results/Plural_-nominal-anaphora\" already exists"
  echo "Please rename or delete it"
  exit 1
fi
if [ -d jsem_results/Nominal-Anaphora ]; then
  echo -e "The directory \"jsem_results/Nominal-Anaphora\" already exists"
  echo "Please rename or delete it"
  exit 1
fi
if [ -d jsem_results/Ellipsis ]; then
  echo -e "The directory \"jsem_results/Nominal-Anaphora\" already exists"
  echo "Please rename or delete it"
  exit 1
fi
if [ -d jsem_results/Adjective ]; then
  echo -e "The directory \"jsem_results/Adjective\" already exists"
  echo "Please rename or delete it"
  exit 1
fi
if [ -d jsem_results/Comparative_-adjective_-temporal-reference ]; then
  echo -e "The directory \"jsem_results/Comparative_-adjective_-temporal-reference\" already exists"
  echo "Please rename or delete it"
  exit 1
fi
if [ -d jsem_results/Temporal-Reference ]; then
  echo -e "The directory \"jsem_results/Temporal-Reference\" already exists"
  echo "Please rename or delete it"
  exit 1
fi
if [ -d jsem_results/Verb ]; then
  echo -e "The directory \"jsem_results/Verb\" already exists"
  echo "Please rename or delete it"
  exit 1
fi
if [ -d jsem_results/Attitude ]; then
  echo -e "The directory \"jsem_results/Attitude\" already exists"
  echo "Please rename or delete it"
  exit 1
fi

# Check if the main result directory exists
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
wait
mv jsem_results/Generalized-Quantifier ${results_dir}/gq
echo "GQ section: Done------------"

echo "------------Plural section"
./evaluate_jsem.sh "Plural" "-nominal anaphora"
wait
mv jsem_results/Plural_-nominal-anaphora ${results_dir}/plural
echo "Plural section: Done------------"

echo "------------Anaphora section"
./evaluate_jsem.sh "Nominal Anaphora"
wait
mv jsem_results/Nominal-Anaphora ${results_dir}/anaphora
echo "Anaphora section: Done------------"

echo "------------Ellipsis section"
./evaluate_jsem.sh "Ellipsis"
wait
mv jsem_results/Ellipsis ${results_dir}/ellipsis
echo "Ellipsis section: Done------------"

echo "------------Adjective section"
./evaluate_jsem.sh "Adjective"
wait
mv jsem_results/Adjective ${results_dir}/adjective
echo "Adjective section: Done------------"

echo "------------Comparative section"
./evaluate_jsem.sh "Comparative" "-adjective" "-temporal reference"
wait
mv jsem_results/Comparative_-adjective_-temporal-reference ${results_dir}/comparative
echo "Comparative section: Done------------"

echo "------------Temporal Reference section"
./evaluate_jsem.sh "Temporal Reference"
wait
mv jsem_results/Temporal-Reference ${results_dir}/tr
echo "Temporal Reference section: Done------------"

echo "------------Verb section"
./evaluate_jsem.sh "Verb"
wait
mv jsem_results/Verb ${results_dir}/verb
echo "Verb section: Done------------"

echo "------------Attitude section"
./evaluate_jsem.sh "Attitude"
wait
mv jsem_results/Attitude ${results_dir}/attitude
echo "Attitude section: Done------------"

### Calculate accuracy, etc. ###
echo "Calculating..."
base_results ${results_dir}/gq/main_jsem.html
mv base_results.txt ${results_dir}/all.results.list

base_results ${results_dir}/plural/main_jsem.html
cat base_results.txt >> ${results_dir}/all.results.list

base_results ${results_dir}/anaphora/main_jsem.html
cat base_results.txt >> ${results_dir}/all.results.list

base_results ${results_dir}/ellipsis/main_jsem.html
cat base_results.txt >> ${results_dir}/all.results.list

base_results ${results_dir}/adjective/main_jsem.html
cat base_results.txt >> ${results_dir}/all.results.list

base_results ${results_dir}/comparative/main_jsem.html
cat base_results.txt >> ${results_dir}/all.results.list

base_results ${results_dir}/tr/main_jsem.html
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

anaphora_speed=`speed 'anaphora'`
anaphora_accuracy=`accuracy 'anaphora'`
anaphora_recall=`recall 'anaphora'`
anaphora_precision=`precision 'anaphora'`
anaphora_gcorrect=`gcorrect 'anaphora'`
anaphora_sysanswer=`sysanswer 'anaphora'`
anaphora_syscorrect=`syscorrect 'anaphora'`
anaphora_correct=`correct 'anaphora'`
anaphora_total=`total 'anaphora'`

ellipsis_speed=`speed 'ellipsis'`
ellipsis_accuracy=`accuracy 'ellipsis'`
ellipsis_recall=`recall 'ellipsis'`
ellipsis_precision=`precision 'ellipsis'`
ellipsis_gcorrect=`gcorrect 'ellipsis'`
ellipsis_sysanswer=`sysanswer 'ellipsis'`
ellipsis_syscorrect=`syscorrect 'ellipsis'`
ellipsis_correct=`correct 'ellipsis'`
ellipsis_total=`total 'ellipsis'`

adjective_speed=`speed 'adjective'`
adjective_accuracy=`accuracy 'adjective'`
adjective_recall=`recall 'adjective'`
adjective_precision=`precision 'adjective'`
adjective_gcorrect=`gcorrect 'adjective'`
adjective_sysanswer=`sysanswer 'adjective'`
adjective_syscorrect=`syscorrect 'adjective'`
adjective_correct=`correct 'adjective'`
adjective_total=`total 'adjective'`

comparative_speed=`speed 'comparative'`
comparative_accuracy=`accuracy 'comparative'`
comparative_recall=`recall 'comparative'`
comparative_precision=`precision 'comparative'`
comparative_gcorrect=`gcorrect 'comparative'`
comparative_sysanswer=`sysanswer 'comparative'`
comparative_syscorrect=`syscorrect 'comparative'`
comparative_correct=`correct 'comparative'`
comparative_total=`total 'comparative'`

tr_speed=`speed 'tr'`
tr_accuracy=`accuracy 'tr'`
tr_recall=`recall 'tr'`
tr_precision=`precision 'tr'`
tr_gcorrect=`gcorrect 'tr'`
tr_sysanswer=`sysanswer 'tr'`
tr_syscorrect=`syscorrect 'tr'`
tr_correct=`correct 'tr'`
tr_total=`total 'tr'`

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

total_number=`echo "(${gq_total} + ${plural_total} + ${anaphora_total} + ${ellipsis_total} + ${adjective_total} + ${comparative_total} + ${tr_total} + ${verb_total} + ${attitude_total})" | bc`

total_accuracy=`echo "scale=4; (${gq_correct} + ${plural_correct} + ${anaphora_correct} + ${ellipsis_correct} + ${adjective_correct} + ${comparative_correct} + ${tr_correct} + ${verb_correct} + ${attitude_correct}) / ${total_number}" | bc -l`

total_recall=`echo "scale=4; (${gq_syscorrect} + ${plural_syscorrect} + ${anaphora_syscorrect} + ${ellipsis_syscorrect} + ${adjective_syscorrect} + ${comparative_syscorrect} + ${tr_syscorrect} + ${verb_syscorrect} + ${attitude_syscorrect}) / (${gq_gcorrect} + ${plural_gcorrect} + ${anaphora_gcorrect} + ${ellipsis_gcorrect} + ${adjective_gcorrect} + ${comparative_gcorrect} + ${tr_gcorrect} + ${verb_gcorrect} + ${attitude_gcorrect})" | bc -l`

total_precision=`echo "scale=4; (${gq_syscorrect} + ${plural_syscorrect} + ${anaphora_syscorrect} + ${ellipsis_syscorrect} + ${adjective_syscorrect} + ${comparative_syscorrect} + ${tr_syscorrect} + ${verb_syscorrect} + ${attitude_syscorrect}) / (${gq_sysanswer} + ${plural_sysanswer} + ${anaphora_sysanswer} + ${ellipsis_sysanswer} + ${adjective_sysanswer} + ${comparative_sysanswer} + ${tr_sysanswer} + ${verb_sysanswer} + ${attitude_sysanswer})" | bc -l`

total_avspeed=`echo "scale=2; ((${gq_speed} * ${gq_total}) + (${plural_speed} * ${plural_total}) + (${anaphora_speed} * ${anaphora_total}) + (${ellipsis_speed} * ${ellipsis_total}) + (${adjective_speed} * ${adjective_total}) + (${comparative_speed} * ${comparative_total}) + (${tr_speed} * ${tr_total}) + (${verb_speed} * ${verb_total}) + (${attitude_speed} * ${attitude_total})) / ${total_number}" | bc -l`


echo -e "-----------------------------------------------------------\n"\
"       Gold |   count| accuracy| recall| precision| av.speed| \n"\
"-----------------------------------------------------------\n"\
"         GQ |     ${gq_total}|    ${gq_accuracy}|  ${gq_recall}|     ${gq_precision}|     ${gq_speed}| \n"\
"     Plural |      ${plural_total}|    ${plural_accuracy}|  ${plural_recall}|     ${plural_precision}|     ${plural_speed}| \n"\
"   Anaphora |      ${anaphora_total}|    ${anaphora_accuracy}|  ${anaphora_recall}|     ${anaphora_precision}|     ${anaphora_speed}| \n"\
"   Ellipsis |      ${ellipsis_total}|    ${ellipsis_accuracy}|  ${ellipsis_recall}|     ${ellipsis_precision}|     ${ellipsis_speed}| \n"\
"  Adjective |      ${adjective_total}|    ${adjective_accuracy}|  ${adjective_recall}|     ${adjective_precision}|     ${adjective_speed}| \n"\
"       Verb |      ${verb_total}|    ${verb_accuracy}|  ${verb_recall}|     ${verb_precision}|     ${verb_speed}|\n"\
"Comparative |      ${comparative_total}|    ${comparative_accuracy}|  ${comparative_recall}|     ${comparative_precision}|     ${comparative_speed}| \n"\
"   Temporal |      ${tr_total}|    ${tr_accuracy}|  ${tr_recall}|     ${tr_precision}|     ${tr_speed}| \n"\
"   Attitude |      ${attitude_total}|    ${attitude_accuracy}|  ${attitude_recall}|     ${attitude_precision}|     ${attitude_speed}|\n"\
"      Total |     ${total_number}|    ${total_accuracy}|  ${total_recall}|     ${total_precision}|     ${total_avspeed}|\n"\
> ${results_dir}/all.results.table

# Create a confusion matrix
./create_conf_matrix.sh ${results_dir}/all.results.list >> ${results_dir}/all.results.table

