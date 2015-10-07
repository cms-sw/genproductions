#!/bin/sh

for pthat in 15 30 50 80 90 100 110 120 170 200 220 250 280 300 370 460 540
do
cat Pythia6_DijetTemplate_pp_TuneZ2_5020GeV_cff.py.txt | sed "s/123456789/${pthat}/g" > Pythia6_Dijet${pthat}_pp_TuneZ2_5020GeV_cfi.py
done
 


