#!/bin/bash

masses=(300 400 500 700)

sample=ChargedHiggs_hw_mh2_200_NLO
postfix=(_run_card.dat _customizecards.dat _proc_card.dat _extramodels.dat _madspin_card.dat)

#echo ${masses[*]}

# get length of an array
tLen=${#postfix[@]}

for mass in ${masses[*]}; do
    echo generating cards for M = $mass GeV
    
    mkdir ${sample}_M${mass}
           
    for (( i=0; i<${tLen}; i++ )) do
#	if [ $(($mass)) -le 500 ] || [ ${postfix[$i]} != _madspin_card.dat ]
#        then
          sed "s/<MASS>/${mass}/g" ${sample}/${sample}${postfix[$i]} > ${sample}_M$mass/${sample}_M$mass${postfix[$i]}
#	fi
    done    
done
