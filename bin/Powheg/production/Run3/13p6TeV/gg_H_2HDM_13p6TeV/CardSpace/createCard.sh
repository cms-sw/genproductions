#!/bin/bash

for mass in 60 65 70 75 80 85 90 95 100 105 110 115 120 125 130 135 140 160 180 200 250 300 350 400 450 500 600 700 800 900 1000 1100 1200 1400 1600 1800 2000 2300 2600 2900 3200 3500;
do
    for hdampmode in "" 1 2;
    do
        for hfactmode in "" 1 2;
        do
            if (( $(echo $mass'<'145 | bc -l) )); 
            then 
                width=0.0041
            elif (( $(echo $mass'<'605 | bc -l) )); then 
                width=0.1
            elif (( $(echo $mass'<'2005 | bc -l) )); then
                width=1.0
            else
                width=2.0
            fi
        
#setup.sh  1: mass   2: channel: incl, t, b or tb # Combinations are (contrib-scale): incl-t/b/tb, t-t, b-b, tb-tb, t-tb, b-tb (!!!now t only)  3. width depending on mass  4.hfactmode
            ./setup.sh $mass t $width $hfactmode; 
        done;
    done;
done;
