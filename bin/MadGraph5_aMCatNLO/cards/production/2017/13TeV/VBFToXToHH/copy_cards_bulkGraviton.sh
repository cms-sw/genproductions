#!/bin/bash

masses=(300 350 400 450 500 550 600 650 700 750 800 900 1000 1200)
sample=VBFToBulkGravitonToHH_M

postfix=(_run_card.dat _customizecards.dat _proc_card.dat _extramodels.dat)

echo ${masses[*]}

for mass in ${masses[*]}; do
    echo generating cards for M = $mass GeV

    mkdir ${sample}${mass}

    for i in {0..3}; do
	sed "s/<MASS>/${mass}/g" ${sample}/${sample}${postfix[$i]} > ${sample}$mass/${sample}$mass${postfix[$i]}
    done
done
