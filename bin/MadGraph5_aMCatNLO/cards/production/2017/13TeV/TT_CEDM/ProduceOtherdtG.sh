#!/bin/bash

firstdtG=1
lastdtG=1
echo "You are creating the cards for the TT_CEDM_dtG process, starting from dtG=$firstdtG GeV to dtG=$lastdtG in steps of 1."

index=0
for i in $(seq $firstdtG 1 $lastdtG)
do 
    cp -r TTbar_LO_dtG0 TTbar_LO_dtG${i}
    mv TTbar_LO_dtG${i}/TTbar_LO_dtG0_customizecards.dat TTbar_LO_dtG${i}/TTbar_LO_dtG${i}_customizecards.dat
    mv TTbar_LO_dtG${i}/TTbar_LO_dtG0_proc_card.dat TTbar_LO_dtG${i}/TTbar_LO_dtG${i}_proc_card.dat
    mv TTbar_LO_dtG${i}/TTbar_LO_dtG0_run_card.dat TTbar_LO_dtG${i}/TTbar_LO_dtG${i}_run_card.dat
    sed -i "s/[0-9]/${i}/g" TTbar_LO_dtG${i}/TTbar_LO_dtG${i}_proc_card.dat
    index=${index}+1
done
