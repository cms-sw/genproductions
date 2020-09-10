#!/bin/bash

scriptname=`basename $0`
EXPECTED_ARGS=2
if [ $# -ne $EXPECTED_ARGS ]
then
    echo "Usage: $scriptname massFileName stringTobeReplaced"
    echo "Example: ./$scriptname update_ATLAS_mass ggProduction"
    exit 1
fi

massfile=$1
string=$2
echo $string

awk -v dir=$string '{print "cp -pr "dir" MH3_"$1"_MH4_"$2"_MH2_"$3"_MHC_"$4}' $massfile | bash

for file in $(ls -d MH3*)
do 
    cd $file
    sed -i 's/'$string'/'$file'/g' *_proc_card.dat
    ../repfil $string $file spacing
    cd -
done

awk '{print "sed -i -e \x27s/MA/"$1"/g\x27 -e \x27s/Ma/"$2"/g\x27 MH3_"$1"_MH4_"$2"_MH2_"$3"_MHC_"$4"/*_customizecards.dat"}' $massfile | bash