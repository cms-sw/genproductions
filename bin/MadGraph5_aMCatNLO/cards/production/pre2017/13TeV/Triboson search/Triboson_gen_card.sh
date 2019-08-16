#!/bin/bash

masses1=(1500)
R1=(0.12 0.2 0.3 0.4 0.5 0.6 0.7 0.9)
masses2=(2000)
R2=(0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.9)
masses3=(2500)
R3=(0.08 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.9)
masses4=(3000 3500 4000 4500 5000)
R4=(0.06 0.08 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.9)
sample=Triboson_M

postfix=(_run_card.dat _customizecards.dat _proc_card.dat _extramodels.dat)

mkdir Cards_for_triboson_resonance_search
cd Cards_for_triboson_resonance_search
for mass in ${masses1[*]}; do
    mkdir ${sample}${mass}
    for R in ${R1[*]}; do

        cd ${sample}${mass}
        echo generating cards for M = ${mass} GeV R = $R
        mkdir ${sample}${mass}_R${R}

        for i in {0..3}; do
            sed  "s/<MASS>/${mass}/g" ../../${sample}_R/${sample}_R${postfix[$i]} > ${sample}${mass}_R$R/${sample}${mass}_R$R${postfix[$i]}
            a=$(echo "$mass*$R"|bc)
            sed -i "s/<MASS_R>/$a/g"  ${sample}${mass}_R$R/${sample}${mass}_R$R${postfix[$i]}
        done
        cd ../
    done
done


for mass in ${masses2[*]}; do
    mkdir ${sample}${mass}
    for R in ${R2[*]}; do

        cd ${sample}${mass}
        echo generating cards for M = ${mass} GeV R = $R
        mkdir ${sample}${mass}_R${R}

        for i in {0..3}; do
            sed  "s/<MASS>/${mass}/g" ../../${sample}_R/${sample}_R${postfix[$i]} > ${sample}${mass}_R$R/${sample}${mass}_R$R${postfix[$i]}
            a=$(echo "$mass*$R"|bc)
            sed -i "s/<MASS_R>/$a/g"  ${sample}${mass}_R$R/${sample}${mass}_R$R${postfix[$i]}
        done
        cd ../
    done
done


for mass in ${masses3[*]}; do
    mkdir ${sample}${mass}
    for R in ${R3[*]}; do

        cd ${sample}${mass}
        echo generating cards for M = ${mass} GeV R = $R
        mkdir ${sample}${mass}_R${R}

        for i in {0..3}; do
            sed  "s/<MASS>/${mass}/g" ../../${sample}_R/${sample}_R${postfix[$i]} > ${sample}${mass}_R$R/${sample}${mass}_R$R${postfix[$i]}
            a=$(echo "$mass*$R"|bc)
            sed -i "s/<MASS_R>/$a/g"  ${sample}${mass}_R$R/${sample}${mass}_R$R${postfix[$i]}
        done
        cd ../
    done
done


for mass in ${masses4[*]}; do
    mkdir ${sample}${mass}
    for R in ${R4[*]}; do

        cd ${sample}${mass}
        echo generating cards for M = ${mass} GeV R = $R
        mkdir ${sample}${mass}_R${R}

        for i in {0..3}; do
            sed  "s/<MASS>/${mass}/g" ../../${sample}_R/${sample}_R${postfix[$i]} > ${sample}${mass}_R$R/${sample}${mass}_R$R${postfix[$i]}
            a=$(echo "$mass*$R"|bc)
            sed -i "s/<MASS_R>/$a/g"  ${sample}${mass}_R$R/${sample}${mass}_R$R${postfix[$i]}
        done
        cd ../
    done
done



