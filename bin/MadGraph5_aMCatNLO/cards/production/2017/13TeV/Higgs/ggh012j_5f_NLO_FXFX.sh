#!/bin/bash

masses=(60 65 70 75 80 85 90 95 100 105 110 120 123 124 125 126 127 130)

sample=ggh012j_5f_NLO_FXFX_

postfix=(_run_card.dat _customizecards.dat _proc_card.dat _extramodels.dat _MadLoopParams.dat)

echo ${masses[*]}

for mass in ${masses[*]}; do
    echo generating cards for M = $mass GeV
    
    mkdir ${sample}${mass}
           
    for i in {0..4}; do
	sed "s/<MASS>/${mass}/g" ${sample}/${sample}${postfix[$i]} > ${sample}$mass/${sample}$mass${postfix[$i]}
    done    
done