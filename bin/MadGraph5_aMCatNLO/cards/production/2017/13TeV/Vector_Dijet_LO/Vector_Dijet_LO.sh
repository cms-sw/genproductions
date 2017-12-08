#!/bin/bash

#masses=(500 750 1000 1250 1500 1750 2000 2500 3000 4000 5000)
masses=(1250 1000)
sample=Vector_Dijet_LO_Mphi

postfix=(_run_card.dat _customizecards.dat _proc_card.dat _madspin_card.dat _reweight_card.dat _extramodels.dat)

echo ${masses[*]}

for mass in ${masses[*]}; do
    echo generating cards for M = $mass GeV
    
    mkdir ${sample}${mass}
           
    for i in {0..5}; do
	sed "s/<MASS>/${mass}/g" ${sample}/${sample}${postfix[$i]} > ${sample}$mass/${sample}$mass${postfix[$i]}
    done    
done