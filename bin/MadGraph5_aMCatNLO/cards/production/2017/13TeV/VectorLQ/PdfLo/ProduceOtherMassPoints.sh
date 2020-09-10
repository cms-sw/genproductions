#!/bin/bash

## If you change the mass range, you need to change also the width vector!!! ##
firstMass=600
lastMass=2000
width=(0.1494 0.1773 0.2049 0.2322 0.2594 0.2864 0.3134 0.3403 0.3672 0.394 0.4207 0.4475 0.4742 0.5009 0.5276)
echo "You are creating the cards for the VectorLQ process, starting from M=$firstMass GeV to M=$lastMass GeV in steps of 100 GeV."

index=0
for i in $(seq $firstMass 100 $lastMass)
do 
    echo "Doing mass $i GeV (width is ${width[${index}]})."
    cp -r VectorLQ_M500 VectorLQ_M${i}
    mv VectorLQ_M${i}/VectorLQ_M500_customizecards.dat VectorLQ_M${i}/VectorLQ_M${i}_customizecards.dat
    mv VectorLQ_M${i}/VectorLQ_M500_extramodels.dat VectorLQ_M${i}/VectorLQ_M${i}_extramodels.dat
    mv VectorLQ_M${i}/VectorLQ_M500_proc_card.dat VectorLQ_M${i}/VectorLQ_M${i}_proc_card.dat
    mv VectorLQ_M${i}/VectorLQ_M500_run_card.dat VectorLQ_M${i}/VectorLQ_M${i}_run_card.dat
    sed -i "s/500/${i}/g" VectorLQ_M${i}/VectorLQ_M${i}_customizecards.dat
    sed -i "s/500/${i}/g" VectorLQ_M${i}/VectorLQ_M${i}_proc_card.dat
    sed -i "s/0.1209/${width[${index}]}/g" VectorLQ_M${i}/VectorLQ_M${i}_customizecards.dat
    index=${index}+1
done