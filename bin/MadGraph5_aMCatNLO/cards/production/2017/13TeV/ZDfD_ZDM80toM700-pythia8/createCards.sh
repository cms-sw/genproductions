#!/bin/sh

#  customizecards.sh
#  For fD Models, scan over ZD
#
#  Created by Jacob Chesslo on 6/30/20.
#
masspoints=(80 91.11876 100 125 200 300 400 500 600 700)
base_dir="2017/13TeV/"
template_dir="ZDfD-pythia8/"

for mp in "${masspoints[@]}"
do
    dir="ZDfD_M${mp}-pythia8/"
    mkdir -p "$dir"
    
    cp "${template_dir}ZDfD-pythia8_extramodels.dat" "${dir}ZDfD_M${mp}-pythia8_extramodels.dat"
    cp "${template_dir}ZDfD-pythia8_proc_card.dat" "${dir}ZDfD_M${mp}-pythia8_proc_card.dat"
    cp "${template_dir}ZDfD-pythia8_run_card.dat" "${dir}ZDfD_M${mp}-pythia8_run_card.dat"
    cp "${template_dir}ZDfD-pythia8_customizecards.dat" "${dir}ZDfD_M${mp}-pythia8_customizecards.dat"    
    
    cd ${dir}
        #customizecard.dat
        sed -i '' "s/MASS/${mp}/g" "ZDfD_M${mp}-pythia8_customizecards.dat"

        #run_card.dat
        sed -i '' "s/EVENTS/5000/g" "ZDfD_M${mp}-pythia8_run_card.dat"
        
        #param_card.dat
        
        #proc_card.dat
        sed -i '' "s/MASS/ZDfD_M${mp}-pythia8/g" "ZDfD_M${mp}-pythia8_proc_card.dat"
        
        #extramodels.dat
        
    cd ../
    
done

