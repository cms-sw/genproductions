#!/bin/bash

 
masses=( M500 M550 M600 M650 M700 M750 M800 M850 M900 M950 M1000 M1100 M1200 M1300 M1400 M1500 )

for p in "${masses[@]}"; do
    echo "************"
    echo "preparing cards for Tprime $p"
    rm -r $p
    mkdir $p

    mass=${p#M}
    
    # Simply update names of extramodels and run card
    cp TprimeTprime_ttHH_extramodels.dat ${p}/TprimeTprime_ttHH_${p}_extramodels.dat
    cp TprimeTprime_ttHH_run_card.dat ${p}/TprimeTprime_ttHH_${p}_run_card.dat
    # Set restricts and output name in proc card
    cp TprimeTprime_ttHH_proc_card.dat ${p}/TprimeTprime_ttHH_${p}_proc_card.dat 
    sed -i s/"default"/"${p}"/g ${p}/TprimeTprime_ttHH_${p}_proc_card.dat
    # Set parameters in customize card
    cp TprimeTprime_ttHH_customize_card.dat ${p}/TprimeTprime_ttHH_${p}_customize_card.dat
    sed -i s/"TprimeM"/"$(printf "%e" $mass)"/g ${p}/TprimeTprime_ttHH_${p}_customize_card.dat
done
echo "************"
