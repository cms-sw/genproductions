#!/bin/bash

for i in 2000 3000 5000; do
    for j in 25 80 170 400; do
	cp -v QstarToQW_M_2000_mW_25_TuneCP2_13TeV_pythia8_cfi.py $(echo QstarToQW_M_2000_mW_25_TuneCP2_13TeV_pythia8_cfi.py | sed "s/2000/$i/" | sed "s/25/$j/")
    done
done

for i in QstarToQW_M_*_mW_*_Tune*.py; do
    ENERGY=$(echo $i | awk -F_ '{ print $3 }')
    MASS=$(echo $i | awk -F_ '{ print $5 }')
    sed -i "s/2000/$ENERGY/" $i
    sed -i "s/25/$MASS/" $i
done

