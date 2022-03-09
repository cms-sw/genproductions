#!/bin/bash

for i in 500 600 800 1200 1400 1600 1800 2000 2500 3000 3500 4000 4500 5000 5500 6000 6500 7000 7500; do
    cp -v QstarToQW_M_1000_TuneCP2_13TeV_pythia8_cfi.py $(echo QstarToQW_M_1000_TuneCP2_13TeV_pythia8_cfi.py | sed "s/1000/$i/")
done

for i in QstarToQW_M_*_TuneCP2_13TeV_pythia8_cfi.py; do
    ENERGY=$(echo $i | awk -F_ '{ print $3 }')
    sed -i "s/1000/$ENERGY/" $i
done