#!/bin/bash

#based on script bin/MadGraph5_aMCatNLO/cards/production/2017/13TeV/BulkGraviton_hh_granular/gen_card.sh

sample=XToYYprime_MX

postfix=(_run_card.dat _customizecards.dat _proc_card.dat _extramodels.dat)

mYprime_l=30
mYprime_r_1=250
mYprime_r_2=500
mYprime_step1=30
mYprime_step2=50
mYprime_step3=100

mY_l=30
mY_step1=30
mY_step2=50
mY_step3=100

mX_l=1000
mX_r=6000
mX_step1=500
mX_step2=1000
mX_tmp=$mX_l

number_of_points=0

mX_step_tmp=$mX_step1
mYprime_r_tmp=$mYprime_r_1
while [ ${mX_tmp} -le ${mX_r} ]
do
    echo generating cards for MX = ${mX_tmp} GeV
    mYprime_tmp=$mYprime_l
    mYprime_step_tmp=$mYprime_step1

    while [ ${mYprime_tmp} -le ${mYprime_r_tmp} ]
    do
        mY_tmp=$mY_l
        mY_step_tmp=$mY_step1
        echo "   "MYprime = ${mYprime_tmp} GeV
        while [ ${mY_tmp} -le ${mYprime_tmp} ]
        do
            echo "      "MY = ${mY_tmp} GeV
            mkdir ${sample}${mX_tmp}_MY${mY_tmp}_MYprime${mYprime_tmp}_narrow
            for i in {0..3}; do
                sed "s/<MASS_X>/${mX_tmp}/g" template_cards/${sample}_MY_MYprime_narrow${postfix[$i]} > ${sample}${mX_tmp}_MY${mY_tmp}_MYprime${mYprime_tmp}_narrow/${sample}${mX_tmp}_MY${mY_tmp}_MYprime${mYprime_tmp}_narrow${postfix[$i]}
                sed -i "s/<MASS_Yprime>/${mYprime_tmp}/g" ${sample}${mX_tmp}_MY${mY_tmp}_MYprime${mYprime_tmp}_narrow/${sample}${mX_tmp}_MY${mY_tmp}_MYprime${mYprime_tmp}_narrow${postfix[$i]}
                sed -i "s/<MASS_Y>/${mY_tmp}/g" ${sample}${mX_tmp}_MY${mY_tmp}_MYprime${mYprime_tmp}_narrow/${sample}${mX_tmp}_MY${mY_tmp}_MYprime${mYprime_tmp}_narrow${postfix[$i]}
            done
        
            if [ ${mY_tmp} -eq 150 ]
            then
                mY_step_tmp=$mY_step2
            elif [ ${mY_tmp} -eq 300 ]
            then
                mY_step_tmp=$mY_step3
            fi

            let "mY_tmp+=mY_step_tmp"
            let "number_of_points=number_of_points+1"
        done
    
        if [ ${mYprime_tmp} -eq 150 ]
        then
            mYprime_step_tmp=$mYprime_step2
        elif [ ${mYprime_tmp} -eq 300 ]
        then
            mYprime_step_tmp=$mYprime_step3
        fi       

        let "mYprime_tmp+=mYprime_step_tmp"
    done

    if [ ${mX_tmp} -eq 2000 ]
    then
        mX_step_tmp=$mX_step2
        mYprime_r_tmp=$mYprime_r_2
    fi

    let "mX_tmp+=mX_step_tmp"
done

echo "Number of mass points: "$number_of_points
