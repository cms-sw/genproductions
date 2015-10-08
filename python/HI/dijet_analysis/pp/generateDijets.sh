#!/bin/sh

for template in Pythia6_bJetTemplate_pp_TuneZ2_5020GeV_cff Pythia6_DijetTemplate_pp_TuneZ2_5020GeV_cff Pythia8_DijetTemplate_pp_TuneCUETP8M1_5020GeV_cff
do
    for pthat in 15 30 50 80 90 100 110 120 170 200 220 250 280 300 370 460 540
    do
	config=`echo $template | sed "s/Template/$pthat/g"`
	cat $template.py.txt | sed "s/123456789/${pthat}/g" > $config.py
    done
done 


