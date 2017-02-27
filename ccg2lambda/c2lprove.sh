#!/bin/bash

# Usage:
# Parse and prove a text in <file>:
#   ./c2lprove <file>
#
# An input text can have multiple lines:
# Premise sentence 1
# Premise sentence 2
# ...
# Conclusion sentence

text=$1

# You need to have a file in the current directory
# named ccg2lambda_location.txt
# where you have the absolute directory path to ccg2lambda.
c2l_dir=`cat ccg2lambda_location.txt`

# You also need to have a file in the current directory
# named semantic_templates_ja_lb.yaml

# Set directory names
lb_dir=`pwd`
parsed_dir="${lb_dir}/parsed"
results_dir="${lb_dir}/results"

if [ ! -d "${parsed_dir}" ]; then
  mkdir $parsed_dir
fi
if [ ! -d "${results_dir}" ]; then
  mkdir $results_dir
fi

# Parse the input text
cat $text | lightblue parse -s xml | ../tidy \
  > ${parsed_dir}/${text}.xml

# Semantic composition via ccg2lambda
python ${c2l_dir}/scripts/semparse.py ${parsed_dir}/${text}.xml \
  semantic_templates_ja_lb.yaml \
  ${parsed_dir}/${text}.sem.xml \
  --arbi-types

# Visualize the parse tree with semantic representation
python ${c2l_dir}/scripts/visualize.py \
  ${parsed_dir}/${text}.sem.xml \
  > ${results_dir}/${text}.sem.html

# # Theorem proving via ccg2lambda
# python ${c2l_dir}/scripts/prove.py \
#   ${parsed}/${text}.sem.xml \
#   --graph_out ${results_dir}/${text}.sem.html
