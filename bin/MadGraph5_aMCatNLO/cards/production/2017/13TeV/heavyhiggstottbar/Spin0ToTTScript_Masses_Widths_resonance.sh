#!/bin/bash

masses=(500 600 700 800 900 1000 1500 2000)
widths=(0.025 0.1 0.25 0.5)

sample_dir=Spin0ToTT_LO_scalar_resonance_
sample=Spin0ToTT_LO_scalar_resonance_

postfix=(run_card.dat customizecards.dat proc_card.dat extramodels.dat)

echo ${masses[*]}
echo ${widths[*]}

for mass in ${masses[*]}; do
    for width_frac in ${widths[*]}; do

        width=$(bc <<< "$mass * $width_frac")

	    echo generating cards for M = $mass GeV and W = $width GeV
    
	    mkdir -p ${sample_dir}"M"${mass}"_W"${width}
        
	    for i in {0..3}; do
	        sed  "s/<MASS>/${mass}/g" ${sample}${postfix[$i]} > ${sample_dir}"M"$mass"_W"${width}/${sample}"M"$mass"_W"${width}"_"${postfix[$i]}
	        sed -i "s/<WIDTH>/${width}/g" ${sample_dir}"M"$mass"_W"${width}/${sample}"M"$mass"_W"${width}"_"${postfix[$i]} 
	    done    
    done
done
