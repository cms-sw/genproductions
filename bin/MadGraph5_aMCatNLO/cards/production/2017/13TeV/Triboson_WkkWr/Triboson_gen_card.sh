#!/bin/bash

masses1=(2000 3000 5000)
R1=(25 80 170 400)
sample=Triboson_M
sample2=_Mr

postfix=(_run_card.dat _customizecards.dat _proc_card.dat _extramodels.dat)

mkdir Cards_for_triboson_resonance_search
cd Cards_for_triboson_resonance_search
for mass in ${masses1[*]}; do
    for R in ${R1[*]}; do
	mkdir ${sample}${mass}${sample2}${R}

        cd ${sample}${mass}${sample2}${R}
        echo generating cards for M = ${mass} GeV R = $R
        #mkdir ${sample}${mass}_R${R}

        for i in {0..3}; do
            #sed  "s/<MASS>/${mass}/g" ../../${sample}_R/${sample}_R${postfix[$i]} > ${sample}${mass}${sample2}${R}/${sample}${mass}${sample2}${R}${postfix[$i]}
	    sed  "s/<MASS>/${mass}/g" ../../${sample}_R/${sample}_R${postfix[$i]} > ./${sample}${mass}${sample2}${R}${postfix[$i]}
            #a=$(echo "$mass*$R"|bc)
            #sed -i "s/<MASS_R>/${R}/g"  ${sample}${mass}_R$R/${sample}${mass}${sample2}${R}${postfix[$i]}
	    sed -i "s/<MASS_R>/${R}/g"  ./${sample}${mass}${sample2}${R}${postfix[$i]}
	    sed -i "s/output Triboson_M3000_R0.5 -nojpeg/output ${sample}${mass}${sample2}${R} -nojpeg/g" ./${sample}${mass}${sample2}${R}${postfix[$i]}
        done
        cd ../
    done
done
