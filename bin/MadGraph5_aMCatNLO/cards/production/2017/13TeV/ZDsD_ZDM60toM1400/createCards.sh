#!/bin/sh

#  customizecards.sh
#  For fD Models, scan over ZD
#
#  Created by Jacob Chesslo on 6/30/20.
#
masspoints=(60 91.11876 100 125 200 300 400 500 600 700 800 900 1000 1100 1200 1300 1400)
base_dir="2017/13TeV/"
template_dir="ZDsD/"

for mp in "${masspoints[@]}"
do
    dir="ZDsD_M${mp}/"
    mkdir -p "$dir"
    
    cp "${template_dir}ZDsD_extramodels.dat" "${dir}ZDsD_M${mp}_extramodels.dat"
    cp "${template_dir}ZDsD_proc_card.dat" "${dir}ZDsD_M${mp}_proc_card.dat"
    cp "${template_dir}ZDsD_run_card.dat" "${dir}ZDsD_M${mp}_run_card.dat"
    cp "${template_dir}ZDsD_customizecards.dat" "${dir}ZDsD_M${mp}_customizecards.dat"
    
    
    cd ${dir}
        #customizecard.dat
        sed -i '' "s/MASS/${mp}/g" "ZDsD_M${mp}_customizecards.dat"

        #run_card.dat
        sed -i '' "s/EVENTS/5000/g" "ZDsD_M${mp}_run_card.dat"
        
        #param_card.dat
        
        #proc_card.dat
        sed -i '' "s/MASS/M${mp}/g" "ZDsD_M${mp}_proc_card.dat"
        
        #extramodels.dat
        
    cd ../
    
done

