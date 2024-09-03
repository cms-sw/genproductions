#!/bin/bash

masses=(60 65 70 75 80 85 90 95 100 105 110 115 120 123 124 125 126 127 130)

echo ${masses[*]}

sample=Hadronizer_TuneCP5_13p6TeV_aMCatNLO_GluGluHToGG_
echo "---> sample:"$sample
for mass in ${masses[*]}; do
    echo generating fragment for M = $mass GeV
	sed "s/<MASS>/${mass}/g" ${sample}.py > ${sample}M-${mass}_LHE_pythia8_cff.py
done

sample=Hadronizer_TuneCP5_13p6TeV_aMCatNLO_VBFHToGG_
echo "---> sample:"$sample
for mass in ${masses[*]}; do
    echo generating fragment for M = $mass GeV
	sed "s/<MASS>/${mass}/g" ${sample}.py > ${sample}M-${mass}_LHE_pythia8_cff.py
done

sample=Hadronizer_TuneCP5_13p6TeV_aMCatNLO_VHToGG_
echo "---> sample:"$sample
for mass in ${masses[*]}; do
    echo generating fragment for M = $mass GeV
	sed "s/<MASS>/${mass}/g" ${sample}.py > ${sample}M-${mass}_LHE_pythia8_cff.py
done

sample=Hadronizer_TuneCP5_13p6TeV_aMCatNLO_ttHToGG_
echo "---> sample:"$sample
for mass in ${masses[*]}; do
    echo generating fragment for M = $mass GeV
	sed "s/<MASS>/${mass}/g" ${sample}.py > ${sample}M-${mass}_LHE_pythia8_cff.py
done