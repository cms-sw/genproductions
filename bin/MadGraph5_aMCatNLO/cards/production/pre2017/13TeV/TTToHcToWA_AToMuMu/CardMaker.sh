#!/bin/bash

##Variable Definitions
InputMhc="130"
InputMA="30"
Var1="MHc"
Var2="MA"
CardVar1="mhc"
CardVar2="mh3"

Process="TTToHcToWA_AToMuMu"

declare -a MhcList=("100" "110" "120" "130" "140" "150" "160")
declare -a MAList=("15" "25" "35" "45" "55" "65" "75")

InputSample=${Process}_${Var1}${InputMhc}_${Var2}${InputMA}_LO
if [[ ! -d ${Process} ]]; then mkdir ${Process}; fi

for Mhc in "${MhcList[@]}"
do
  for MA in "${MAList[@]}"
  do
    DeltaM=$(( ${Mhc} - ${MA} ))
    if [ ${DeltaM} -lt 80 ] 
    then 
      continue
    fi

    DirName=${Process}_${Var1}${Mhc}_${Var2}${MA}_LO
    echo ${DirName}
    cp -r ${InputSample} ${Process}/${DirName}

    if [ ${InputMhc} -eq ${Mhc} ]
    then
       if [ ${InputMA} -eq ${MA} ]
       then
          continue
       fi
    fi

    mv ${Process}/${DirName}/${InputSample}_run_card.dat ${Process}/${DirName}/${DirName}_run_card.dat
    mv ${Process}/${DirName}/${InputSample}_proc_card.dat ${Process}/${DirName}/${DirName}_proc_card.dat
    mv ${Process}/${DirName}/${InputSample}_customizecards.dat ${Process}/${DirName}/${DirName}_customizecards.dat
    
    sed -i "s/${InputSample}/${DirName}/g" ${Process}/${DirName}/${DirName}_proc_card.dat
    sed -i -e "s/${CardVar1}\ ${InputMhc}/${CardVar1}\ ${Mhc}/g" -e "s/${CardVar2}\ ${InputMA}/${CardVar2}\ ${MA}/g" ${Process}/${DirName}/${DirName}_customizecards.dat
  done
done
