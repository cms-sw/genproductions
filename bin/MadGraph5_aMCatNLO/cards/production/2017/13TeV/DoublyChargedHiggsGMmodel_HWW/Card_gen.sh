#!/bin/bash

for mass in 200 300 400 500 600 700 800 900 1000 1500 2000 3000
do
    echo $mass
    mkdir -p DoublyChargedHiggsGMmodel_HWW_M${mass}
    
    cp DoublyChargedHiggsGMmodel_HWW_run_card.dat DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_run_card.dat
    
    cp DoublyChargedHiggsGMmodel_HWW_customizecards.dat DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat

    cp DoublyChargedHiggsGMmodel_HWW_proc_card.dat DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_proc_card.dat

    sed -i 's|output DoublyChargedHiggsGMmodel_HWW|output DoublyChargedHiggsGMmodel_HWW_M'"${mass}"'|g' DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_proc_card.dat

    if [ ${mass} == 200 ]
    then 
	sed -i 's|set param_card WH5pp 8.785|set param_card WH5pp 0.26025|g' DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
	sed -i 's|set param_card WH5p 8.785|set param_card WH5p 0.26025|g' DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
	sed -i 's|set param_card M1coeff 275|set param_card M1coeff 40.5|g' DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
	sed -i 's|set param_card M2coeff 326|set param_card M2coeff 40.5|g' DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
    elif [ ${mass} == 300 ]
    then 
	sed -i 's|set param_card WH5pp 8.785|set param_card WH5pp 1.4485|g' DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
	sed -i 's|set param_card WH5p 8.785|set param_card WH5p 1.4485|g' DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
	sed -i 's|set param_card M1coeff 275|set param_card M1coeff 230.0|g' DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
	sed -i 's|set param_card M2coeff 326|set param_card M2coeff 41.5|g' DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
    elif [ ${mass} == 400 ]
    then 
	sed -i 's|set param_card WH5pp 8.785|set param_card WH5pp 4.125|g' DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
	sed -i 's|set param_card WH5p 8.785|set param_card WH5p 4.125|g' DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
	sed -i 's|set param_card M1coeff 275|set param_card M1coeff 260.0|g' DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
	sed -i 's|set param_card M2coeff 326|set param_card M2coeff 161.0|g' DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
    elif [ ${mass} == 500 ]
    then 
	sed -i 's|set param_card WH5pp 8.785|set param_card WH5pp 8.785|g' DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
	sed -i 's|set param_card WH5p 8.785|set param_card WH5p 8.785|g' DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
	sed -i 's|set param_card M1coeff 275|set param_card M1coeff 275|g' DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
	sed -i 's|set param_card M2coeff 326|set param_card M2coeff 326|g' DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
        echo 'set param_card lam4 1' >> DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
    elif [ ${mass} == 600 ]
    then 
	sed -i 's|set param_card WH5pp 8.785|set param_card WH5pp 15.9125|g' DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
	sed -i 's|set param_card WH5p 8.785|set param_card WH5p 15.9125|g' DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
	sed -i 's|set param_card M1coeff 275|set param_card M1coeff 400.0|g' DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
	sed -i 's|set param_card M2coeff 326|set param_card M2coeff 473.915016|g' DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
        echo 'set param_card lam4 1' >> DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
    elif [ ${mass} == 700 ]
    then 
	sed -i 's|set param_card WH5pp 8.785|set param_card WH5pp 26.0|g' DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
	sed -i 's|set param_card WH5p 8.785|set param_card WH5p 26.0|g' DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
	sed -i 's|set param_card M1coeff 275|set param_card M1coeff 650.0|g' DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
	sed -i 's|set param_card M2coeff 326|set param_card M2coeff 598.0312|g' DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
        echo 'set param_card lam4 1' >> DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
    elif [ ${mass} == 800 ]
    then 
	sed -i 's|set param_card WH5pp 8.785|set param_card WH5pp 39.525|g' DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
	sed -i 's|set param_card WH5p 8.785|set param_card WH5p 39.525|g' DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
	sed -i 's|set param_card M1coeff 275|set param_card M1coeff 850.0|g' DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
	sed -i 's|set param_card M2coeff 326|set param_card M2coeff 785.473|g' DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
        echo 'set param_card lam4 1' >> DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
    elif [ ${mass} == 900 ]
    then 
	sed -i 's|set param_card WH5pp 8.785|set param_card WH5pp 57.0|g' DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
	sed -i 's|set param_card WH5p 8.785|set param_card WH5p 57.0|g' DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
	sed -i 's|set param_card M1coeff 275|set param_card M1coeff 1000.0|g' DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
	sed -i 's|set param_card M2coeff 326|set param_card M2coeff 1036.24|g' DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
        echo 'set param_card lam4 1' >> DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
    elif [ ${mass} == 1000 ]
    then 
	sed -i 's|set param_card WH5pp 8.785|set param_card WH5pp 78.925|g' DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
	sed -i 's|set param_card WH5p 8.785|set param_card WH5p 78.925|g' DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
	sed -i 's|set param_card M1coeff 275|set param_card M1coeff 1200.0|g' DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
	sed -i 's|set param_card M2coeff 326|set param_card M2coeff 1300.333|g' DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
        echo 'set param_card lam4 1' >> DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
    elif [ ${mass} == 1500 ]
    then 
	sed -i 's|set param_card WH5pp 8.785|set param_card WH5pp 10.89|g' DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
	sed -i 's|set param_card WH5p 8.785|set param_card WH5p 10.89|g' DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
	sed -i 's|set param_card M1coeff 275|set param_card M1coeff 1220.0|g' DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
	sed -i 's|set param_card M2coeff 326|set param_card M2coeff 1341.44935|g' DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
        sed -i 's|set param_card tanth 0.57735|set param_card tanth 0.1005037815|g' DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
    elif [ ${mass} == 2000 ]
    then 
	sed -i 's|set param_card WH5pp 8.785|set param_card WH5pp 26.0|g' DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
	sed -i 's|set param_card WH5p 8.785|set param_card WH5p 26.0|g' DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
	sed -i 's|set param_card M1coeff 275|set param_card M1coeff 2140.0|g' DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
	sed -i 's|set param_card M2coeff 326|set param_card M2coeff 2928.88658|g' DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
        sed -i 's|set param_card tanth 0.57735|set param_card tanth 0.1005037815|g' DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
    elif [ ${mass} == 3000 ]
    then 
	sed -i 's|set param_card WH5pp 8.785|set param_card WH5pp 1.0|g' DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
	sed -i 's|set param_card WH5p 8.785|set param_card WH5p 1.0|g' DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
	sed -i 's|set param_card M1coeff 275|set param_card M1coeff 4900|g' DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
	sed -i 's|set param_card M2coeff 326|set param_card M2coeff 5225|g' DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
        sed -i 's|set param_card tanth 0.57735|set param_card tanth 0.1005037815|g' DoublyChargedHiggsGMmodel_HWW_M${mass}/DoublyChargedHiggsGMmodel_HWW_M${mass}_customizecards.dat
    fi
done
