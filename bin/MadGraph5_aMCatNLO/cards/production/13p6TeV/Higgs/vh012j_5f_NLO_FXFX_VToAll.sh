#!/bin/bash

masses=(60 65 70 75 80 85 90 95 100 105 110 120 123 124 125 126 127 130)

sample=vh012j_5f_NLO_FXFX_M

postfix=(_run_card.dat _customizecards.dat _proc_card.dat _madspin_card.dat)

echo ${masses[*]}

for mass in ${masses[*]}; do
    echo generating cards for M = $mass GeV
    
    mkdir ${sample}${mass}_VToAll
           
    for i in {0..3}; do
	sed "s/<MASS>/${mass}/g" ${sample}_VToAll/${sample}_VToAll${postfix[$i]} > ${sample}${mass}_VToAll/${sample}${mass}_VToAll${postfix[$i]}
    done    
done