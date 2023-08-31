#!/bin/bash

 
masses=( M250 M300 M350 )

for p in "${masses[@]}"; do
    echo "************"
    echo "preparing cards for 2HDM h2 $p"
    rm -r $p
    mkdir $p

    mass=${p#M}
    
    # Simply update names of extramodels and run card
    cp ttHH_2HDM_extramodels.dat ${p}/ttHH_2HDM_${p}_extramodels.dat
    cp ttHH_2HDM_run_card.dat ${p}/ttHH_2HDM_${p}_run_card.dat
    # Set output name in proc card
    cp ttHH_2HDM_proc_card.dat ${p}/ttHH_2HDM_${p}_proc_card.dat 
    sed -i s/"default"/"${p}"/g ${p}/ttHH_2HDM_${p}_proc_card.dat
    # Set parameters in customize card
    cp ttHH_2HDM_customize_card.dat ${p}/ttHH_2HDM_${p}_customize_card.dat
    sed -i s/"H2mass"/"$(printf "%e" $mass)"/g ${p}/ttHH_2HDM_${p}_customize_card.dat
done
echo "************"