#!/bin/sh

for tune in Wide Narrow Coll FullWide FullNarrow
  do
  for pthat in 15 30 50 80 90 100 110 120 170 200 220 250 280 300 370 460 540
    do

    coll=True
    rad=True
    spec=1

    if [ $tune == "Narrow" ] || [ $tune == "FullNarrow" ]; then
	spec=0
    fi
    
    if [ $tune == "Narrow" ] || [ $tune == "Wide" ]; then
        coll=False
    fi
    
    if [ $tune == "Coll" ]; then
        rad=False
    fi

    cat Pyquen_DijetTemplate_NN_Quenched_TuneD6T_2760GeV_cfi.py.txt | sed "s/123456789/${pthat}/g" | sed "s/RAD/$rad/g" | sed "s/COLL/$coll/g" | sed "s/SPEC/$spec/g" > Pyquen${tune}_Dijet${pthat}_NN_Quenched_TuneD6T_2760GeV_cfi.py
  done
done



