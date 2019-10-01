#!/bin/bash
sample=BulkGraviton_hh_GF_HH_narrow_MX

postfix=(_run_card.dat _customizecards.dat _proc_card.dat _extramodels.dat)

mkdir BulkGraviton_hh_GF_HH
cd BulkGraviton_hh_GF_HH
mH_l=50
mH_r=250
mH_step=10
mX_l=600
mX_r=6000
mX_step=100
mH_tmp=$mH_l
while [ ${mH_tmp} -le ${mH_r} ]
do
    mkdir ${sample}_MH${mH_tmp}
    echo generating cards for MH = ${mH_tmp} GeV
    mX_tmp=$mX_l
    while [ ${mX_tmp} -le ${mX_r} ]
    do
        mkdir ${sample}_MH${mH_tmp}/${sample}${mX_tmp}_MH${mH_tmp}
        for i in {0..3}; do
            sed "s/<MASS_X>/${mX_tmp}/g" ../example_Cards/${sample}_MH${postfix[$i]} > ${sample}_MH${mH_tmp}/${sample}${mX_tmp}_MH${mH_tmp}/${sample}${mX_tmp}_MH${mH_tmp}${postfix[$i]}
            sed -i "s/<MASS_H>/${mH_tmp}/g" ${sample}_MH${mH_tmp}/${sample}${mX_tmp}_MH${mH_tmp}/${sample}${mX_tmp}_MH${mH_tmp}${postfix[$i]}
        done
        let "mX_tmp+=mX_step"
    done
    let "mH_tmp+=mH_step"
done
cd ../

