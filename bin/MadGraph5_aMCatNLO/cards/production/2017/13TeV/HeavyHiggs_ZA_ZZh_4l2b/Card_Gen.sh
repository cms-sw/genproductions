#!/bin/bash

for mass in 350_220 400_220 450_220 500_220 550_220 600_220 650_220 700_220 750_220 800_220 350_250 400_250 450_250 500_250 550_250 600_250 650_250 700_250 750_250 800_250 400_290 450_290 500_290 550_290 600_290 650_290 700_290 750_290 800_290
do
    echo $mass
    mkdir -p HeavyHiggs_ZA_ZZh_4l2b_M${mass}

    
    cp HeavyHiggs_ZA_ZZh_4l2b_run_card.dat HeavyHiggs_ZA_ZZh_4l2b_M${mass}/HeavyHiggs_ZA_ZZh_4l2b_M${mass}_run_card.dat
    
    cp HeavyHiggs_ZA_ZZh_4l2b_customizecards.dat HeavyHiggs_ZA_ZZh_4l2b_M${mass}/HeavyHiggs_ZA_ZZh_4l2b_M${mass}_customizecards.dat

    cp HeavyHiggs_ZA_ZZh_4l2b_extramodels.dat HeavyHiggs_ZA_ZZh_4l2b_M${mass}/HeavyHiggs_ZA_ZZh_4l2b_M${mass}_extramodels.dat

    massH=${mass%_*}
    massA=${mass##*_}

    sed -i 's|35 4.000000e+02 # mH2|35 '"${massH}"' # mH2|g' HeavyHiggs_ZA_ZZh_4l2b_M${mass}/HeavyHiggs_ZA_ZZh_4l2b_M${mass}_customizecards.dat

    sed -i 's|36 2.500000e+02 # mAs|36 '"${massA}"' # mAs|g' HeavyHiggs_ZA_ZZh_4l2b_M${mass}/HeavyHiggs_ZA_ZZh_4l2b_M${mass}_customizecards.dat

    sed -i 's|37 2.500000e+02 # mcH|37 '"${massA}"' # mcH|g' HeavyHiggs_ZA_ZZh_4l2b_M${mass}/HeavyHiggs_ZA_ZZh_4l2b_M${mass}_customizecards.dat

    cp HeavyHiggs_ZA_ZZh_4l2b_proc_card.dat HeavyHiggs_ZA_ZZh_4l2b_M${mass}/HeavyHiggs_ZA_ZZh_4l2b_M${mass}_proc_card.dat
    sed -i 's|output HeavyHiggs_ZA_ZZh_4l2b -nojpeg|output HeavyHiggs_ZA_ZZh_4l2b_M'"${mass}"' -nojpeg|g' HeavyHiggs_ZA_ZZh_4l2b_M${mass}/HeavyHiggs_ZA_ZZh_4l2b_M${mass}_proc_card.dat
done
