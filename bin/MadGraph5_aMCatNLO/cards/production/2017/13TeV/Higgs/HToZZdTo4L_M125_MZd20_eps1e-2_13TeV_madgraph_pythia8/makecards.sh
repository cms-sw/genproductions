#!/bin/bash
###########
### PURPOSE: Quickly generate MG cards for various Zdark mass points.
### SYNTAX: ./makecards.sh
### NOTES: User should verify epsilon, lhaid used, and mass points below. 
###########

eps="1e-2"
lhaid=306000

for zdmass in 1 2 3 4 7 10 15 20 25 30 35
do
    cp -r HAHMcards_eps_MZD_lhaid_template/ HAHMcards_eps${eps}_MZD${zdmass}_lhaid${lhaid}/ 
    cd HAHMcards_eps${eps}_MZD${zdmass}_lhaid${lhaid}/
    sed -i "s/MASS/${zdmass}/g" HAHM_variablesw_v3_customizecards.dat
    sed -i "s/LHAID/${lhaid}/g" HAHM_variablesw_v3_run_card.dat
    cd ..
done
