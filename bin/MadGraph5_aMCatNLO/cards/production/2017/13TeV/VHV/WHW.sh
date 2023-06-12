#!/bin/bash

masses=(260 280 300 400 500 600 700 800 900 1000 1200 1400 1600 2000 2500 3000 3500 4000)

sample=WHtoWhh
postfix=(_run_card.dat _customizecards.dat _proc_card.dat _extramodels.dat)

#echo ${masses[*]}

# get length of an array
tLen=${#postfix[@]}

for mass in ${masses[*]}; do
    echo generating cards for M = $mass GeV
    
    mkdir ${sample}_mH${mass}
           
    for (( i=0; i<${tLen}; i++ )) do
          sed "s/<MASS>/${mass}/g" ${sample}/${sample}${postfix[$i]} > ${sample}_mH$mass/${sample}_mH$mass${postfix[$i]}
    done    
done