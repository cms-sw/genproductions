#!/bin/bash

for mass in 075 080 085 090 100 110 120 130 140 150 160
do
    echo $mass
    mkdir -p ChargedHiggsToCB_M${mass}_LO
    
    cp ChargedHiggsToCB_M120_run_card.dat ChargedHiggsToCB_M${mass}_LO/ChargedHiggsToCB_M${mass}_run_card.dat
    
    cp ChargedHiggsToCB_M120_customizecards.dat ChargedHiggsToCB_M${mass}_LO/ChargedHiggsToCB_M${mass}_customizecards.dat
    sed -i 's|set param_card mass 37 120|set param_card mass 37 '"${mass}"'|g' ChargedHiggsToCB_M${mass}_LO/ChargedHiggsToCB_M${mass}_customizecards.dat

    cp ChargedHiggsToCB_M120_proc_card.dat ChargedHiggsToCB_M${mass}_LO/ChargedHiggsToCB_M${mass}_proc_card.dat
    sed -i 's|output ChargedHiggsToCB_M120 -nojpeg|output ChargedHiggsToCB_M'"${mass}"' -nojpeg|g' ChargedHiggsToCB_M${mass}_LO/ChargedHiggsToCB_M${mass}_proc_card.dat


done
