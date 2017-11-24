#!/bin/bash

process=$1
firstMass=$2
lastMass=$3
echo "You are creating the cards for the $process process, starting from M=$firstMass GeV to M=$lastMass GeV in steps of 100 GeV."

for i in $(seq $firstMass 100 $lastMass)
do 
    echo "Doing mass $i GeV."
    cp -r tZb_NW/${process}_M700 tZb_NW/${process}_M${i}
    mv tZb_NW/${process}_M${i}/${process}_M700_customizecards.dat tZb_NW/${process}_M${i}/${process}_M${i}_customizecards.dat
    mv tZb_NW/${process}_M${i}/${process}_M700_extramodels.dat tZb_NW/${process}_M${i}/${process}_M${i}_extramodels.dat
    mv tZb_NW/${process}_M${i}/${process}_M700_madspin_card.dat tZb_NW/${process}_M${i}/${process}_M${i}_madspin_card.dat
    mv tZb_NW/${process}_M${i}/${process}_M700_proc_card.dat tZb_NW/${process}_M${i}/${process}_M${i}_proc_card.dat
    mv tZb_NW/${process}_M${i}/${process}_M700_run_card.dat tZb_NW/${process}_M${i}/${process}_M${i}_run_card.dat
    sed -i "s/700/${i}/g" tZb_NW/${process}_M${i}/${process}_M${i}_customizecards.dat
done