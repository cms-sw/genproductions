#!/bin/sh

for pthat in 15 30 50 80 90 100 110 120 170 200 220 250 280 300 370 460 540
do
cat Pyquen_DijetTemplate_NN_Unquenched_TuneZ2_2760GeV_cfi.py.txt | sed "s/123456789/${pthat}/g" > Pyquen_Dijet${pthat}_NN_Unquenched_TuneZ2_2760GeV_cfi.py
cat Pyquen_DijetTemplate_NN_Unquenched_TuneZ2_2760GeV_cff.py.txt | sed "s/123456789/${pthat}/g" > Pyquen_Dijet${pthat}_NN_Unquenched_TuneZ2_2760GeV_cff.py
done
 


