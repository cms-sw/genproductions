#!/bin/bash

masses=(1200 1300 1400 1500 1600 1700 1800 1900 2000 2100 2200 2300 2400)

sample=TpbTobW_LO_

postfix=(run_card.dat customizecards.dat proc_card.dat)

echo ${masses[*]}

for mass in ${masses[*]}; do
    echo generating cards for M = $mass GeV
    
    mkdir ${sample}"M"${mass}
           
    for i in {0..2}; do
	sed "s/<MASS>/${mass}/g" ${sample}${postfix[$i]} > ${sample}"M"$mass/${sample}"M"$mass"_"${postfix[$i]}
    done    
done
