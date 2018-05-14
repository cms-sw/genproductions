#!/bin/bash

masses=(120 125 130)

procname=vbfH
sample=_NLO_HtoElElGamma

postfix=(_run_card.dat _customizecards.dat _proc_card.dat _extramodels.dat _madspin_card.dat _FKS_params.dat)

echo ${masses[*]}

# get length of postfix array
tLen=${#postfix[@]}

for mass in ${masses[*]}; do
    echo generating cards for M = $mass GeV
    
    echo "${procname}${mass}${sample}"
    # creating new dir e.g ggH120_012j_NLO_FXFX_HtoMuMuGamma
    mkdir -p ${procname}${mass}${sample}
           
    for ((i=0; i<${tLen}; i++)); do
	
	sed "s/<MASS>/${mass}/g" ${procname}${sample}/${procname}${sample}${postfix[$i]} > ${procname}${mass}${sample}/${procname}${mass}${sample}${postfix[$i]}
	
    done
    
    if [ $mass -eq 120 -o $mass -eq 130 ]; then 
	echo "hey edit the file: ", ${procname}${mass}${sample}/${procname}${mass}${sample}_extramodels.dat
	#sed -i '' "s/.*TEXT_TO_BE_REPLACED.*/XYZ/" file.txt 
	#sed -i '' "s/.*HC_NLO_X0_UFO-lepton_masses_no_.*/HC_NLO_X0_UFO-lepton_masses_no_lepton_yukawas_MX0_${mass}-v1.3.tar.gz/" ${procname}${mass}${sample}/${procname}${mass}${sample}_extramodels.dat #OK for MAC
	sed -i "s/.*HC_NLO_X0_UFO-lepton_masses_no_.*/HC_NLO_X0_UFO-lepton_masses_no_lepton_yukawas_MX0_${mass}-v1.3.tar.gz/" ${procname}${mass}${sample}/${procname}${mass}${sample}_extramodels.dat #OK for Linux
	
    fi	
done
