rm result.csv
rm test.csv
time=1
echo "file""\t""result""\t""note""\t""limit : $time" >> "result.csv"
for file in `ls DTS/Alligator/TPTP/*.test`; do
#for file in `ls ../../ILTP-v1.1.2-propositional/Problems/SYN/*.p`; do
    gtimeout -sKILL $time ./DTS/Alligator/AlexHappy/FileParser "${file}" > "result.txt"
    #echo $file
    if [ $? != 0 ]; then
	echo  "$file""\t""\t""timeout false" >> "result.csv"
    else
	result=$(cat "result.txt")
	echo $result
	echo  "$file""\t""$result" >> "result.csv"
    fi
done


for file in `ls ../../ILTP-v1.1.2-propositional/Problems/SYN/*.p`; do
    gtimeout -sKILL $time ./DTS/Alligator/AlexHappy/FileParser "${file}" > "result.txt"
    #echo $file
    if [ $? != 0 ]; then
	echo  "$file""\t""\t""timeout false" >> "result.csv"
    else
	result=$(cat "result.txt")
	echo $result
	echo  "$file""\t""$result" >> "result.csv"
    fi
done

rm result.txt
