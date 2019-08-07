#!/bin/bash

## If you change the mass range, you need to ALSO change the widths !!! ##Not including 300, as that already exists as the example
masses=(300 400 500 600 700 800 900 1000 1100 1200 1300 1400 1500 1600 1700 1800 1900 2000 2100 2200 2300 2400 2500 2600 2700 2800 2900 3000 3500 4000)
## width is ( yukawa^2 * m_LQ / 16pi )
## this assumes yukawa=1, but you should set coupling to whatever you want
widths=(5.968 7.958 9.947 11.937 13.926 15.915 17.905 19.894 21.884 23.873 25.863 27.852 29.842 31.831 33.820 35.810 37.799 39.789 41.778 43.768 45.757 47.746 49.736 51.725 53.715 55.704 57.694 59.683 69.630 79.577)
## this assumes yukawa=0.3
#widths=(0.537 0.716 0.895 1.074 1.253 1.432 1.611 1.790 1.970 2.149 2.328 2.507 2.686 2.865 3.044 3.223 3.402 3.581 3.760 3.939 4.118 4.297 4.476 4.655 4.834 5.013 5.192 5.371 6.267 7.162)
echo "You are creating the cards for the singly produced LQ process, ranging from M=${masses[0]} GeV to M=${masses[${#masses[@]}-1]} GeV."

index=1

until [ $index -eq ${#masses[@]} ]; do
    echo "Doing mass ${masses[$index]} GeV (width is ${widths[${index}]})."
    #exit
    echo "cp -r LQToBMu_madgraph_LO_single-M300 LQToBMu_madgraph_LO_single-M${masses[$index]}"

    cp -r LQToBMu_madgraph_LO_single-M300 LQToBMu_madgraph_LO_single-M${masses[$index]}
    mv LQToBMu_madgraph_LO_single-M${masses[$index]}/LQToBMu_madgraph_LO_single-M300_customizecards.dat LQToBMu_madgraph_LO_single-M${masses[$index]}/LQToBMu_madgraph_LO_single-M${masses[$index]}_customizecards.dat
    mv LQToBMu_madgraph_LO_single-M${masses[$index]}/LQToBMu_madgraph_LO_single-M300_extramodels.dat LQToBMu_madgraph_LO_single-M${masses[$index]}/LQToBMu_madgraph_LO_single-M${masses[$index]}_extramodels.dat
    mv LQToBMu_madgraph_LO_single-M${masses[$index]}/LQToBMu_madgraph_LO_single-M300_proc_card.dat LQToBMu_madgraph_LO_single-M${masses[$index]}/LQToBMu_madgraph_LO_single-M${masses[$index]}_proc_card.dat
    mv LQToBMu_madgraph_LO_single-M${masses[$index]}/LQToBMu_madgraph_LO_single-M300_run_card.dat LQToBMu_madgraph_LO_single-M${masses[$index]}/LQToBMu_madgraph_LO_single-M${masses[$index]}_run_card.dat
    mv LQToBMu_madgraph_LO_single-M${masses[$index]}/LQToBMu_madgraph_LO_single-M300_param_card.dat LQToBMu_madgraph_LO_single-M${masses[$index]}/LQToBMu_madgraph_LO_single-M${masses[$index]}_param_card.dat
    sed -i "s/300/${masses[$index]}/g" LQToBMu_madgraph_LO_single-M${masses[$index]}/LQToBMu_madgraph_LO_single-M${masses[$index]}_customizecards.dat
    sed -i "s/300/${masses[$index]}/g" LQToBMu_madgraph_LO_single-M${masses[$index]}/LQToBMu_madgraph_LO_single-M${masses[$index]}_proc_card.dat
    sed -i "s/300/${masses[$index]}/g" LQToBMu_madgraph_LO_single-M${masses[$index]}/LQToBMu_madgraph_LO_single-M${masses[$index]}_run_card.dat
    sed -i "s/300/${masses[$index]}/g" LQToBMu_madgraph_LO_single-M${masses[$index]}/LQToBMu_madgraph_LO_single-M${masses[$index]}_param_card.dat
    sed -i "s/5.968/${widths[${index}]}/g" LQToBMu_madgraph_LO_single-M${masses[$index]}/LQToBMu_madgraph_LO_single-M${masses[$index]}_param_card.dat
    sed -i "s/5.968/${widths[${index}]}/g" LQToBMu_madgraph_LO_single-M${masses[$index]}/LQToBMu_madgraph_LO_single-M${masses[$index]}_customizecards.dat
    let index=${index}+1

done
