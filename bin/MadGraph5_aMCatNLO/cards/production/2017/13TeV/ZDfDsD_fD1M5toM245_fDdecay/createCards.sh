#!/bin/sh

#  customizecards.sh
#  For fD Models, scan over ZD
#
#  Created by Jacob Chesslo on 6/30/20.
#
masspoints=(80 91.11876 100 125 200 300 400 500 600 700)
base_dir="2017/13TeV/"
template_dir="ZDfDsD_fD1scan_fDdecay/"

for mp in "${masspoints[@]}"
do
    dir="ZDfDsD_fD1scan_fDdecay_M${mp}/"
    mkdir -p "$dir"
    
    cp "${template_dir}ZDfDsD_fD1scan_fDdecay_extramodels.dat" "${dir}ZDfDsD_fD1scan_fDdecay_M${mp}_extramodels.dat"
    cp "${template_dir}ZDfDsD_fD1scan_fDdecay_proc_card.dat" "${dir}ZDfDsD_fD1scan_fDdecay_M${mp}_proc_card.dat"
    cp "${template_dir}ZDfDsD_fD1scan_fDdecay_run_card.dat" "${dir}ZDfDsD_fD1scan_fDdecay_M${mp}_run_card.dat"
    cp "${template_dir}ZDfDsD_fD1scan_fDdecay_customizecards.dat" "${dir}ZDfDsD_fD1scan_fDdecay_M${mp}_customizecards.dat"
    
    
    cd ${dir}
        #customizecard.dat
        sed -i '' "s/MASS/${mp}/g" "ZDfDsD_fD1scan_fDdecay_M${mp}_customizecards.dat"

        #run_card.dat
        sed -i '' "s/EVENTS/5000/g" "ZDfDsD_fD1scan_fDdecay_M${mp}_run_card.dat"
        
        #param_card.dat
        
        #proc_card.dat
        sed -i '' "s/MASS/M${mp}/g" "ZDfDsD_fD1scan_fDdecay_M${mp}_proc_card.dat"
        
        #extramodels.dat
        
    cd ../
    
done

