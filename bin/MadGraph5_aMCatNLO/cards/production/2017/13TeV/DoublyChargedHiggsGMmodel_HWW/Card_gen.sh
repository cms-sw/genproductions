#!/bin/bash

mass_hp5=(200 300 400 500 600 700 800 900 1000 1500 2000 3000)
width_hp5=(0.26025 1.4485 4.125 8.785 15.9125 26.0 39.525 57.0 78.925 10.89 26.0 1.0)
M1=(40.5 230.0 260.0 275 400.0 650.0 850.0 1000.0 1200.0 1220.0 2140.0 4900)
M2=(40.5 41.5 161.0 326 473.915016 598.0312 785.473 1036.24 1300.333 1341.44935 2928.88658 5225)
num=0

for mass in ${mass_hp5[@]}
do
    echo $mass 
    mkdir -p DoublyChargedHiggsGMmodel_HWW_M${mass}
    
    cp DoublyChargedHiggsGMmodel_HWW_run_card.dat DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_run_card.dat
    
    cp DoublyChargedHiggsGMmodel_HWW_customizecards.dat DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat

    cp DoublyChargedHiggsGMmodel_HWW_proc_card.dat DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_proc_card.dat

    sed -i 's|output DoublyChargedHiggsGMmodel_HWW|output DoublyChargedHiggsGMmodel_HWW_M'"${mass}"'|g' DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_proc_card.dat
    sed -i "s|set param_card WH5pp 8.785|set param_card WH5pp ${width_hp5[num]}|g" DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
    sed -i "s|set param_card WH5p 8.785|set param_card WH5p ${width_hp5[num]}|g" DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat	
    sed -i "s|set param_card M1coeff 275|set param_card M1coeff ${M1[num]}|g" DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
    sed -i "s|set param_card M2coeff 326|set param_card M2coeff ${M2[num]}|g" DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
 
    if [ ${mass} == 500 ] || [ ${mass} == 600 ] || [ ${mass} == 700 ] || [ ${mass} == 800 ] || [ ${mass} == 900 ] || [ ${mass} == 1000 ]
    then 
        echo 'set param_card lam4 1' >> DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
    fi
    
    if [ ${mass} == 1500 ] || [ ${mass} == 2000 ] || [ ${mass} == 3000 ]
    then 
        sed -i 's|set param_card tanth 0.57735|set param_card tanth 0.1005037815|g' DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
    fi

    num=`expr $num + 1`
done
