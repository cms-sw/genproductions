#!/bin/bash

masses=(145 150 155 160 165)

sample=ChargedHiggs_taunu_intermediateNoNeutral_LO
postfix=(_run_card.dat _customizecards.dat _proc_card.dat _extramodels.dat)

#echo ${masses[*]}

# get length of an array
tLen=${#postfix[@]}

for mass in ${masses[*]}; do
    echo generating cards for M = $mass GeV
    
    mkdir ${sample}_M${mass}
           
    for (( i=0; i<${tLen}; i++ )) do
	sed "s/<MASS>/${mass}/g" ${sample}/${sample}${postfix[$i]} > ${sample}_M$mass/${sample}_M$mass${postfix[$i]}
    done    
done
