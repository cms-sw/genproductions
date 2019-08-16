#!/bin/bash

process1=$1
process2=$2
firstMass=$3
lastMass=$4
width=$5
echo "You are creating the cards for the $process1/$process2 process, starting from M=$firstMass GeV to M=$lastMass GeV in steps of 100 GeV."

for i in $(seq $firstMass 100 $lastMass)
do 
    echo "Doing mass $i GeV."
    if [ $width == -1 ]; then
	cp -r ${process1}/${process2}_M700 ${process1}/${process2}_M${i}
	mv ${process1}/${process2}_M${i}/${process2}_M700_customizecards.dat ${process1}/${process2}_M${i}/${process2}_M${i}_customizecards.dat
	mv ${process1}/${process2}_M${i}/${process2}_M700_extramodels.dat ${process1}/${process2}_M${i}/${process2}_M${i}_extramodels.dat
	mv ${process1}/${process2}_M${i}/${process2}_M700_madspin_card.dat ${process1}/${process2}_M${i}/${process2}_M${i}_madspin_card.dat
	mv ${process1}/${process2}_M${i}/${process2}_M700_proc_card.dat ${process1}/${process2}_M${i}/${process2}_M${i}_proc_card.dat
	mv ${process1}/${process2}_M${i}/${process2}_M700_run_card.dat ${process1}/${process2}_M${i}/${process2}_M${i}_run_card.dat
	sed -i "s/700/${i}/g" ${process1}/${process2}_M${i}/${process2}_M${i}_customizecards.dat
	sed -i "s/700/${i}/g" ${process1}/${process2}_M${i}/${process2}_M${i}_proc_card.dat
    else
	cp -r ${process1}/${process2}_M700GeV_W10p ${process1}/${process2}_M${i}GeV_W${width}p
	mv ${process1}/${process2}_M${i}GeV_W${width}p/${process2}_M700GeV_W10p_customizecards.dat ${process1}/${process2}_M${i}GeV_W${width}p/${process2}_M${i}GeV_W${width}p_customizecards.dat
	mv ${process1}/${process2}_M${i}GeV_W${width}p/${process2}_M700GeV_W10p_extramodels.dat ${process1}/${process2}_M${i}GeV_W${width}p/${process2}_M${i}GeV_W${width}p_extramodels.dat
	mv ${process1}/${process2}_M${i}GeV_W${width}p/${process2}_M700GeV_W10p_madspin_card.dat ${process1}/${process2}_M${i}GeV_W${width}p/${process2}_M${i}GeV_W${width}p_madspin_card.dat
	mv ${process1}/${process2}_M${i}GeV_W${width}p/${process2}_M700GeV_W10p_proc_card.dat ${process1}/${process2}_M${i}GeV_W${width}p/${process2}_M${i}GeV_W${width}p_proc_card.dat
	mv ${process1}/${process2}_M${i}GeV_W${width}p/${process2}_M700GeV_W10p_run_card.dat ${process1}/${process2}_M${i}GeV_W${width}p/${process2}_M${i}GeV_W${width}p_run_card.dat
	NewWidth=$(( i * width / 100 ))
	sed -i "s/700.0/${i}.0/g" ${process1}/${process2}_M${i}GeV_W${width}p/${process2}_M${i}GeV_W${width}p_customizecards.dat
	sed -i "s/70.0/${NewWidth}.0/g" ${process1}/${process2}_M${i}GeV_W${width}p/${process2}_M${i}GeV_W${width}p_customizecards.dat
	sed -i "s/700/${i}/g" ${process1}/${process2}_M${i}GeV_W${width}p/${process2}_M${i}GeV_W${width}p_proc_card.dat
	sed -i "s/10/${width}/g" ${process1}/${process2}_M${i}GeV_W${width}p/${process2}_M${i}GeV_W${width}p_proc_card.dat
    fi
done