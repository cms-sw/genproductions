#!/bin/bash

masses=(60 65 70 75 80 85 90 95 100 105 110 120 123 124 125 126 127 130)

sample=thq_4f_ckm_LO_ctcvcp_MH
postfix=(_run_card.dat _customizecards.dat _proc_card.dat _extramodels.dat _reweight_card.dat)

echo ${masses[*]}

# get length of an array
tLen=${#postfix[@]}

for mass in ${masses[*]}; do
    echo generating cards for M = $mass GeV
    
    mkdir ${sample}${mass}
           
    for (( i=0; i<${tLen}; i++ )) do
	sed "s/<MASS>/${mass}/g" ${sample}/${sample}${postfix[$i]} > ${sample}$mass/${sample}$mass${postfix[$i]}
    done    
done