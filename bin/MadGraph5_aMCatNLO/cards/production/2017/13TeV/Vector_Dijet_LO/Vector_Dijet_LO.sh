#!/bin/bash

masses=(10 30 50 75 100 115 125 150 175 200 225 250 275 300 350 450 500 600)
sample=Vector_Dijet_LO_Mphi

postfix=(_run_card.dat _customizecards.dat _proc_card.dat _madspin_card.dat _reweight_card.dat _extramodels.dat)

echo ${masses[*]}

for mass in ${masses[*]}; do
    echo generating cards for M = $mass GeV
    
    mkdir -p ${sample}${mass}
           
    for i in {0..5}; do
	sed "s/<MASS>/${mass}/g" ${sample}/${sample}${postfix[$i]} > ${sample}$mass/${sample}$mass${postfix[$i]}
    done    
done