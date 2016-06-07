#!/bin/bash

# Usage:
# Parse and prove a sentence:
#   ./myprove.sh <input> [OPTION]
#
# Example:
#   ./myprove.sh "太郎がロンドンに住んでいるなら、太郎はロンドンに住んでいる" -latex -show 
#   ./myprove.sh <file> -latex -show 
#
# -latex
#     Create a latex output file named "test.tex":
# -show
#     Show the preterm, prolog input, etc:

input=$1
arg2=${2:-"none"}
arg3=${3:-"none"}

if [ -e $input ];then
  all_results=`cat $input | ./DTStoProlog`
else
  all_results=`echo $input | ./DTStoProlog`
fi

results=`echo $all_results \
  | sed 's/-- Preterm ---------/##/g' \
  | sed 's/-- Prolog input ---------/##/g' \
  | sed 's/-- After resolving @ --------/##/g' \
  | sed 's/-- After elimSigma --------/##/g' \
  | sed 's/-- Coq formula --------/##/g' \
  | sed 's/-- Coq code --------/##/g' \
  | sed 's/-- Answer --------/##/g'`

preterm=`echo $results | awk -F"##" '{print $2;}'`
prolog=`echo $results | awk -F"##" '{print $3;}'`
resolved=`echo $results | awk -F"##" '{print $4;}'`
normal=`echo $results | awk -F"##" '{print $5;}'`
coqformula=`echo $results | awk -F"##" '{print $6;}'`
signatureLine=`echo $results | awk -F"##" '{print $7;}'`
signature=`echo $signatureLine | sed 's/\./\.#/g' | tr '#' '\n' | sed 's/^ //g'`

answer=`echo $results | awk -F"##" '{print $NF;}'`
echo "answer:${answer}"

# output=`echo -e "Require Export coqlib.\n \
#   $signature\n \
#   Theorem trm: ${coqformula}. \
#   firstorder. Qed." | coqtop 2>/dev/null`

coqscript="Require Export coqlib.\n$signature\nTheorem trm: ${coqformula}. firstorder. Qed."

if [ $arg2 = "-show" -o $arg3 = "-show" ]; then
echo -e "--- Preterm ---\n${preterm}"
echo -e "--- Prolog input ---\n${prolog}"
echo -e "--- Resolving @ ---\n${resolved}"
echo -e "--- Normalized ---\n${normal}"
# echo -e "--- Coq formula ---\n${coqformula}"
# echo -e "--- Coq signature ---\n${signature}"
echo -e "--- Coq script ---\n${signature}"
# echo -e "--- Coq script ---\n${coqscript}"
fi

# echo $output | awk '{if ($NF == "defined") {print "yes"} else {print "no"}}'
# echo $output | awk '{if ( $0 ~ /is defined/ ) {print "yes"} else {print "no"}}'

g1=`echo $prolog | sed 's/_//g'`
g2=`echo $resolved | sed 's/_//g'`
g3=`echo $normal | sed 's/_//g'`

if [ $arg2 = '-latex' -o $arg3 = '-latex' ]; then
  swipl -s Prolog/prolog2latex.pl -g main -t halt --quiet -- "${g1}" "${g2}" "${g3}"
  # platex test.tex > /dev/null 2>&1
  # dvipdfmx test.dvi > /dev/null 2>&1
  # open test.pdf
fi
