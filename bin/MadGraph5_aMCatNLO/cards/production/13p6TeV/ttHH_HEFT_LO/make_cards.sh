#!/bin/bash

parameters=( "c2_m1" "c2_6" "c2_3" "kl_2" "kl_3" "kt_2" "kl_0p5_c2_1" "kl_1" )
# parameters=( "kl_0p5_c2_1" )

for p in "${parameters[@]}"; do
    echo "************"
    echo "preparing cards for $p"
    rm -r $p
    mkdir $p

    IFS='_' read -r -a parray <<< "$p"

    if [ ${#parray[@]} -gt 2 ]; then #setting 2 parameters at once 
        label=${parray[0]}"_"${parray[2]}
        params=( ${parray[0]} ${parray[2]} )
        values=( ${parray[1]} ${parray[3]} )
    else # or just 1 
        label=${parray[0]}
        params=( ${parray[0]} )
        values=( ${parray[1]} ) 
    fi
    
    # Simply update names of extramodels and run card
    cp ttHH_HEFT_extramodels.dat ${p}/ttHH_HEFT_${p}_extramodels.dat
    cp ttHH_HEFT_run_card.dat ${p}/ttHH_HEFT_${p}_run_card.dat
    # Set restricts and output name in proc card
    cp ttHH_HEFT_proc_card.dat ${p}/ttHH_HEFT_${p}_proc_card.dat 
    sed -i s/"default"/"${p}"/g ${p}/ttHH_HEFT_${p}_proc_card.dat
    echo "setting restrict to "$label
    sed -i s/"restrict"/$label/g ${p}/ttHH_HEFT_${p}_proc_card.dat
    # Set parameters in customize card
    cp ttHH_HEFT_customize_card.dat ${p}/ttHH_HEFT_${p}_customize_card.dat
    for i in `seq 1 ${#values[@]}`; do 
        value=${values[i-1]}
        value=${value//p/.}
        value=${value//m/-}
        echo "setting "${params[$i-1]}" to "$value 
        pline=($(awk /${params[$i-1]}/ ${p}/ttHH_HEFT_${p}_customize_card.dat))   
        sed -i /${pline[3]}/s/${pline[4]}/$(printf "%e" $value)/g ${p}/ttHH_HEFT_${p}_customize_card.dat

    done
done
echo "************"
