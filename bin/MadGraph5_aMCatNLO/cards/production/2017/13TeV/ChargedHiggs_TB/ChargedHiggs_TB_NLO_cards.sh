#!/bin/bash

masses=(200 220 250 300 350 400 500 600 700 800 1000 1250 1500 1750 2000 2500 3000)

sample=ChargedHiggs_TB_NLO
postfix=(_run_card.dat _customizecards.dat _proc_card.dat _extramodels.dat)

#echo ${masses[*]}

# get length of an array
tLen=${#postfix[@]}

for mass in ${masses[*]}; do
    echo generating cards for M = $mass GeV

    for (( i=0; i<${tLen}; i++ )) do
        if [ ! -d ${sample}_M${mass} ]; then
            mkdir ${sample}_M${mass}
        fi
        sed "s/<MASS>/${mass}/g" ${sample}/${sample}${postfix[$i]} > ${sample}_M$mass/${sample}_M$mass${postfix[$i]}
    done  
done
