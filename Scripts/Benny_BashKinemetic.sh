#! /bin/sh

cd /Users/pcnnc/Documents/Opto_data/Scripts/
 
while getopts :p:s:r:t:n: params
do
		case $params in
		n ) subnum="$OPTARG"  ;;  # subject number

    esac
done

if [ "$subnum" == "" ]; then
echo "Usage: `basename $0` [-n subject number]"
exit 1
fi

#for subnum in 04 05 12 13 14 15 16 17 18 19 20 21 22 24 25 26 28 31 32 33 34 35 36 37 38 39 41; do

mkdir -pv /Users/pcnnc/Documents/Opto_data/InProgress/03_${subnum}/{output,original,QA}

echo "Moving original data"
cp /Users/pcnnc/Documents/Opto_data/dataOpto/03_${subnum}/03_${subnum}.txt /Users/pcnnc/Documents/Opto_data/InProgress/03_${subnum}/original
cp /Users/pcnnc/Documents/Opto_data/dataOpto/03_${subnum}/03_${subnum}.txt /Users/pcnnc/Documents/Opto_data/InProgress/03_${subnum}/original
cp /Users/pcnnc/Documents/Opto_data/dataOptoPresentation/03_${subnum}/03_${subnum}_optoReplant.txt /Users/pcnnc/Documents/Opto_data/InProgress/03_${subnum}/original
cp /Users/pcnnc/Documents/Opto_data/dataOptoPresentation/03_${subnum}/03_${subnum}_optoReplantHuman.txt /Users/pcnnc/Documents/Opto_data/InProgress/03_${subnum}/original
cp /Users/pcnnc/Documents/Opto_data/dataOptoPresentation/03_${subnum}/03_${subnum}-opto_replant.log /Users/pcnnc/Documents/Opto_data/InProgress/03_${subnum}/original
chmod -R g+rw .

#Changing subject numbers in R and save as a new template
echo "changing R script subject number"

cd /Users/pcnnc/Documents/Opto_data/Scripts/

sed -e 's@+subj+@'${subnum}'@g' /Users/pcnnc/Documents/Opto_data/Scripts/Benny_MoCap_Template_p1.R > /Users/pcnnc/Documents/Opto_data/InProgress/03_${subnum}/original/03_${subnum}_MoCap_Template_p1.R

sed -e 's@+subj+@'${subnum}'@g' /Users/pcnnc/Documents/Opto_data/Scripts/Benny_MoCapFunctions.R > /Users/pcnnc/Documents/Opto_data/InProgress/03_${subnum}/original/03_${subnum}_MoCapFunctions.R

# Execute Part 1 R script
echo "Starting Executing R script part 1"

Rscript /Users/pcnnc/Documents/Opto_data/InProgress/03_${subnum}/original/03_${subnum}_MoCap_Template_p1.R

echo "finished"

mv /Users/pcnnc/Documents/Opto_data/InProgress/03_${subnum}/original/03_${subnum}_outputnumber.csv /Users/pcnnc/Documents/Opto_data/InProgress/03_${subnum}/output
for i in 1 2 3 4; do
mv /Users/pcnnc/Documents/Opto_data/InProgress/03_${subnum}/original/b${i}_9.csv /Users/pcnnc/Documents/Opto_data/InProgress/03_${subnum}/QA
mv /Users/pcnnc/Documents/Opto_data/InProgress/03_${subnum}/original/b${i}_8.csv /Users/pcnnc/Documents/Opto_data/InProgress/03_${subnum}/QA
done

#done

