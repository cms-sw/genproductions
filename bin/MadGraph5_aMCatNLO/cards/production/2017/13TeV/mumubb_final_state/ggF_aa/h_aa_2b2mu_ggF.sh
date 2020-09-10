#!/bin/bash

masses=(10 15 20 25 30 35 40 45 50 55  60)
sample=ggh01_M125_Toa01a01_Tomumubb_M

postfix=(_run_card.dat _customizecards.dat _proc_card.dat _extramodels.dat)

echo ${masses[*]}

for mass in ${masses[*]}; do
    echo generating cards for M = $mass GeV

    mkdir ${sample}${mass}
    for ((i=0;i<${#postfix[@]};i+=1)); do 
   #for value in {1..4}; do 
     #echo $value
     #done           
	sed "s/<MASS>/${mass}/g" ${sample}/${sample}${postfix[$i]} > ${sample}$mass/${sample}$mass${postfix[$i]}
        done
    done    
