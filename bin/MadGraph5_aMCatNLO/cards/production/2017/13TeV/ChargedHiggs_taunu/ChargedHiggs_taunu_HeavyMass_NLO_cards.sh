#!/bin/bash

masses=(500 750 1000 1500 2000 2500 3000 5000 7000 10000)

sample=ChargedHiggs_taunu_heavy_NLO
heavysample=ChargedHiggs_taunu_HeavyMass_NLO
postfix=(_run_card.dat _customizecards.dat _proc_card.dat _extramodels.dat)

#echo ${masses[*]}

# get length of an array
tLen=${#postfix[@]}

for mass in ${masses[*]}; do
    echo generating cards for M = $mass GeV
    
    mkdir ${heavysample}_M${mass}
           
    for (( i=0; i<${tLen}; i++ )) do
        sed "s/<MASS>/${mass}/g" ${sample}/${sample}${postfix[$i]} > ${heavysample}_M$mass/${heavysample}_M$mass${postfix[$i]}
	sed -i "s/${sample}/${heavysample}/" ${heavysample}_M$mass/${heavysample}_M$mass${postfix[$i]}
    done    
done
