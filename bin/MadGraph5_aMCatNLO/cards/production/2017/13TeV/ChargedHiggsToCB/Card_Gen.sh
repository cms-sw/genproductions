#!/bin/bash

for mass in 020 030 040 050 060 070 075 080 085 090 100 110 120 125 130 140 150 160
do
    echo $mass
    mkdir -p ChargedHiggsToCB_M${mass}
    
    cp ChargedHiggsToCB_M160_run_card.dat ChargedHiggsToCB_M${mass}/ChargedHiggsToCB_M${mass}_run_card.dat
    
    cp ChargedHiggsToCB_M160_customizecards.dat ChargedHiggsToCB_M${mass}/ChargedHiggsToCB_M${mass}_customizecards.dat
    sed -i 's|set param_card mass 37 1.600000e+02|set param_card mass 37 '"${mass}"'|g' ChargedHiggsToCB_M${mass}/ChargedHiggsToCB_M${mass}_customizecards.dat

    cp ChargedHiggsToCB_M160_proc_card.dat ChargedHiggsToCB_M${mass}/ChargedHiggsToCB_M${mass}_proc_card.dat
    sed -i 's|output ChargedHiggsToCB_M160 -nojpeg|output ChargedHiggsToCB_M'"${mass}"' -nojpeg|g' ChargedHiggsToCB_M${mass}/ChargedHiggsToCB_M${mass}_proc_card.dat
done