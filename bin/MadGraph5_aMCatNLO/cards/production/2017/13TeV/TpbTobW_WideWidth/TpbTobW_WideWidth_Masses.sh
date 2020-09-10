#!/bin/bash

masses=(1200 1300 1400 1500 1600 1700 1800 1900 2000 2100 2200 2300 2400)
widths=(10 20 30)

sample=TpbTobW_WideWidth_

postfix=(run_card.dat customizecards.dat proc_card.dat extramodels.dat)

echo ${masses[*]}
echo ${widths[*]}

for mass in ${masses[*]}; do
    for wid in ${widths[*]}; do
      echo generating cards for M = $mass GeV
    
      mkdir -p ${sample}"W"${wid}"_M"${mass}
      width=$((($mass*$wid)/100))
      echo Mass width $width
      for i in {0..3}; do
	  sed  "s/<MASS>/${mass}/g;s/<WID>/${width}/g;s/<WIDTH>/${wid}/g" ${sample}${postfix[$i]} > ${sample}"W"${wid}"_M"$mass/${sample}"W"${wid}"_M"$mass"_"${postfix[$i]}
      done    
    done
done
