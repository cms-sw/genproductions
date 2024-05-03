#!/bin/bash

masses=(10 20 30 40 50 60)
sample=vbfh01_M125_Toa01a01_Tomumubb_M

postfix=(_run_card.dat _customizecards.dat _proc_card.dat _extramodels.dat)

echo ${masses[*]}

for mass in ${masses[*]}; do
    echo generating cards for M = $mass GeV

    mkdir ${sample}${mass}
    for ((i=0;i<${#postfix[@]};i+=1)); do           
	sed "s/<MASS>/${mass}/g" ${sample}/${sample}${postfix[$i]} > ${sample}$mass/${sample}$mass${postfix[$i]}
        done
    done    
