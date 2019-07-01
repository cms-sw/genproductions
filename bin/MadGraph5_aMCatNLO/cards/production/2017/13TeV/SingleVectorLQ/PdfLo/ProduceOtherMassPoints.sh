#!/bin/bash

## If you change the mass range, you need to change also the width vector!!! ##
firstMass=600
lastMass=2000
width=(0.1494 0.1773 0.2049 0.2322 0.2594 0.2864 0.3134 0.3403 0.3672 0.394 0.4207 0.4475 0.4742 0.5009 0.5276)
echo "You are creating the cards for the SingleVectorLQ process, starting from M=$firstMass GeV to M=$lastMass GeV in steps of 100 GeV."

index=0
for i in $(seq $firstMass 100 $lastMass)
do 
    echo "Doing mass $i GeV (width is ${width[${index}]})."
    cp -r SingleVectorLQ_M500 SingleVectorLQ_M${i}
    mv SingleVectorLQ_M${i}/SingleVectorLQ_M500_customizecards.dat SingleVectorLQ_M${i}/SingleVectorLQ_M${i}_customizecards.dat
    mv SingleVectorLQ_M${i}/SingleVectorLQ_M500_extramodels.dat SingleVectorLQ_M${i}/SingleVectorLQ_M${i}_extramodels.dat
    mv SingleVectorLQ_M${i}/SingleVectorLQ_M500_proc_card.dat SingleVectorLQ_M${i}/SingleVectorLQ_M${i}_proc_card.dat
    mv SingleVectorLQ_M${i}/SingleVectorLQ_M500_run_card.dat SingleVectorLQ_M${i}/SingleVectorLQ_M${i}_run_card.dat
    sed -i "s/500/${i}/g" SingleVectorLQ_M${i}/SingleVectorLQ_M${i}_customizecards.dat
    sed -i "s/500/${i}/g" SingleVectorLQ_M${i}/SingleVectorLQ_M${i}_proc_card.dat
    sed -i "s/0.1209/${width[${index}]}/g" SingleVectorLQ_M${i}/SingleVectorLQ_M${i}_customizecards.dat
    index=${index}+1
done