#!/bin/bash

mass_hp5=(200 300 400 500 600 700 800 900 1000 1500 2000 3000)
width_hp5=(0.26025 1.4485 4.125 8.785 15.9125 26.0 39.525 57.0 78.925 10.89 26.0 1.0)
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

    if [ ${mass} == 200 ]
    then 
	sed -i 's|set param_card M1coeff 275|set param_card M1coeff 40.5|g' DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
	sed -i 's|set param_card M2coeff 326|set param_card M2coeff 40.5|g' DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
    elif [ ${mass} == 300 ]
    then 
	sed -i 's|set param_card M1coeff 275|set param_card M1coeff 230.0|g' DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
	sed -i 's|set param_card M2coeff 326|set param_card M2coeff 41.5|g' DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
    elif [ ${mass} == 400 ]
    then 
	sed -i 's|set param_card M1coeff 275|set param_card M1coeff 260.0|g' DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
	sed -i 's|set param_card M2coeff 326|set param_card M2coeff 161.0|g' DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
    elif [ ${mass} == 500 ]
    then 
	sed -i 's|set param_card M1coeff 275|set param_card M1coeff 275|g' DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
	sed -i 's|set param_card M2coeff 326|set param_card M2coeff 326|g' DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
        echo 'set param_card lam4 1' >> DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
    elif [ ${mass} == 600 ]
    then 
	sed -i 's|set param_card M1coeff 275|set param_card M1coeff 400.0|g' DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
	sed -i 's|set param_card M2coeff 326|set param_card M2coeff 473.915016|g' DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
        echo 'set param_card lam4 1' >> DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
    elif [ ${mass} == 700 ]
    then 
	sed -i 's|set param_card M1coeff 275|set param_card M1coeff 650.0|g' DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
	sed -i 's|set param_card M2coeff 326|set param_card M2coeff 598.0312|g' DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
        echo 'set param_card lam4 1' >> DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
    elif [ ${mass} == 800 ]
    then 
	sed -i 's|set param_card M1coeff 275|set param_card M1coeff 850.0|g' DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
	sed -i 's|set param_card M2coeff 326|set param_card M2coeff 785.473|g' DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
        echo 'set param_card lam4 1' >> DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
    elif [ ${mass} == 900 ]
    then 
	sed -i 's|set param_card M1coeff 275|set param_card M1coeff 1000.0|g' DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
	sed -i 's|set param_card M2coeff 326|set param_card M2coeff 1036.24|g' DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
        echo 'set param_card lam4 1' >> DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
    elif [ ${mass} == 1000 ]
    then 
	sed -i 's|set param_card M1coeff 275|set param_card M1coeff 1200.0|g' DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
	sed -i 's|set param_card M2coeff 326|set param_card M2coeff 1300.333|g' DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
        echo 'set param_card lam4 1' >> DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
    elif [ ${mass} == 1500 ]
    then 
	sed -i 's|set param_card M1coeff 275|set param_card M1coeff 1220.0|g' DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
	sed -i 's|set param_card M2coeff 326|set param_card M2coeff 1341.44935|g' DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
        sed -i 's|set param_card tanth 0.57735|set param_card tanth 0.1005037815|g' DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
    elif [ ${mass} == 2000 ]
    then 
	sed -i 's|set param_card M1coeff 275|set param_card M1coeff 2140.0|g' DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
	sed -i 's|set param_card M2coeff 326|set param_card M2coeff 2928.88658|g' DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
        sed -i 's|set param_card tanth 0.57735|set param_card tanth 0.1005037815|g' DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
    elif [ ${mass} == 3000 ]
    then 
	sed -i 's|set param_card M1coeff 275|set param_card M1coeff 4900|g' DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
	sed -i 's|set param_card M2coeff 326|set param_card M2coeff 5225|g' DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
        sed -i 's|set param_card tanth 0.57735|set param_card tanth 0.1005037815|g' DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
    fi
    num=`expr $num + 1`
done
