#!/bin/bash

# Based on https://github.com/cms-sw/genproductions/blob/master/bin/MadGraph5_aMCatNLO/cards/production/2017/13TeV/XToYYprime/gen_card.sh

sample=XtoYYprimeTo4Q

cards="run_card.dat customizecards.dat proc_card.dat extramodels.dat"

mX_all="2000 3000 5000"
mY_all="25 80 170 400"

for mX in $mX_all
do
    for mY in $mY_all
    do
        for mYprime in $mY_all
        do
            # mY must be >= mYprime
            if [ $mY -lt $mYprime ]
            then
                continue
            fi

            name="${sample}_Par-MX-${mX}GeV-MY-${mY}GeV-MYprime-${mYprime}GeV" #_TuneCP5_13p6TeV_madgraph-pythia8
            echo "Generating cards for $name"
            mkdir -p $name
            for card in $cards
            do
                sed    "s/<MASS_X>/$mX/g" $card >$name/${name}_$card
                sed -i "s/<MASS_Y>/$mY/g" $name/${name}_$card
                sed -i "s/<MASS_Yprime>/$mYprime/g" $name/${name}_$card
                sed -i "s/<PROCESS>/$name/g" $name/${name}_$card
            done
        done
    done
done
