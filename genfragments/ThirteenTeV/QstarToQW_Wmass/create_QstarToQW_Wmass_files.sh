#!/bin/bash

for i in 2000 3000 5000; do
    for j in 25 80 170 400; do
	cp -v QstarToQW_M_2000_mW_25_TuneCP2_13TeV_pythia8_cfi.py $(echo QstarToQW_M_2000_mW_25_TuneCP2_13TeV_pythia8_cfi.py | sed "s/2000/$i/" | sed "s/25/$j/")
    done
done

for i in QstarToQW_M_*_mW_*_TuneCP*.py; do
    ENERGY=$(echo $i | awk -F_ '{ print $3 }')
    MASS=$(echo $i | awk -F_ '{ print $5 }')
    sed -i "s/2000/$ENERGY/" $i
    sed -i "s/mWidth = 2/mWidth = $((1*${ENERGY}/1000))/" $i
    sed -i "s/m0 = 25/m0 = $MASS/" $i
    if [ $MASS -ne 25 ]
    then
	sed -i "s/mWidth = 0.625/mWidth = $((2*${MASS}/80))/" $i
    fi
done

