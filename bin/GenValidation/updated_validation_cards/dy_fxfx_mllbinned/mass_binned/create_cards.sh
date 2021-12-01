#!/bin/bash

masses=(10 50 100 200 500 700 800 1000 1500 2000 3000) #as per https://pdmv-pages.web.cern.ch/main_bkg_ul/

for((i=0;i<${#masses[*]} - 1;i++))
do
    lowMass=${masses[i]}
    highMass=${masses[i+1]}
    
    dir=dy${lowMass}to${highMass}_fxfx
    echo generating cards for ${dir}
    mkdir ${dir} 
    #copy inclusive cards
    cp ../mll10to50/*custo* ${dir}/dyellell012j_mll${lowMass}to${highMass}_5f_NLO_FXFX_customizecards.dat 
    cp ../mll10to50/*proc* ${dir}/dyellell012j_mll${lowMass}to${highMass}_5f_NLO_FXFX_proc_card.dat
    #replace inside
    sed -i "9s/dyellell012j_mll10to50_5f_NLO_FXFX/dyellell012j_mll${lowMass}to${highMass}_5f_NLO_FXFX/g" ${dir}/*proc_card.dat
    
    # copy other cards
    cp ../mll10to50/*run* ${dir}/dyellell012j_mll${lowMass}to${highMass}_5f_NLO_FXFX_run_card.dat
    #replace the "10.0  = mll_sf" with corresponding low cut
    sed -i "145s/10.0  = mll_sf/${lowMass}.0  = mll_sf/g" ${dir}/*run_card.dat

    #copy cuts.f file
    cp ../mll10to50/*cuts* ${dir}/dyellell012j_mll${lowMass}to${highMass}_5f_NLO_FXFX_cuts.f 
    sed -i "143s/gt.50/gt.${highMass}/g" ${dir}/*_cuts.f
    
done
