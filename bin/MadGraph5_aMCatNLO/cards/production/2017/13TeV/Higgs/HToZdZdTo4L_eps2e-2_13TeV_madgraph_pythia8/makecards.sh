#!/bin/bash
###########
### PURPOSE: Quickly generate MG cards for various Zdark mass points.
### SYNTAX: ./makecards.sh
### NOTES: User should verify epsilon, lhaid used, and mass points below. 
###########

eps="2e-2"
lhaid=306000
cardsDir="HToZdZdcards_lhaid306000_eps2e-2_template"

for zdmass in 4 5 6 7 8 9 10 15 20 25 30 35 40 45 50 55 60; do
    cp -r ${cardsDir}/ HToZdZdcards_eps${eps}_MZD${zdmass}_lhaid${lhaid}/ 
    cd HToZdZdcards_eps${eps}_MZD${zdmass}_lhaid${lhaid}/
    sed -i "s/ZDMASS/${zdmass}/g" HAHM_variablesw_v3_customizecards.dat
    sed -i "s/LHAID/${lhaid}/g" HAHM_variablesw_v3_run_card.dat
    cd ..
done
