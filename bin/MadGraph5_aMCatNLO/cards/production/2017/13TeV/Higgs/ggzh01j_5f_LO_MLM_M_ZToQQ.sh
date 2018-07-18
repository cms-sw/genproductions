#!/bin/bash

masses=(120 123 124 125 126 127 130)

sample=ggzh01j_5f_LO_MLM_M

samplesufix=_ZToQQ

postfix=(_run_card.dat _customizecards.dat _proc_card.dat)

echo ${masses[*]}

for mass in ${masses[*]}; do
    echo generating cards for M = $mass GeV
    
    mkdir -p ${sample}${mass}${samplesufix}
           
    for i in {0..2}; do
	sed "s/<MASS>/${mass}/g" ${sample}${samplesufix}/${sample}${samplesufix}${postfix[$i]} > ${sample}${mass}${samplesufix}/${sample}${mass}${samplesufix}${postfix[$i]}
    done    
done
