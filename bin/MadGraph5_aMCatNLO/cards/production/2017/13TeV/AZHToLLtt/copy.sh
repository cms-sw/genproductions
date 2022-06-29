#!/bin/bash

default=$(basename $(pwd))

is=(1000 2100 900 500 2100 1500 1500 2100 500 700 1200 1800 500 700 900 700 900 1200 1500 1800 2100 800 800 1000 1000 1200)
js=(600 2000 350 400 1000 1400 1000 1600 350 350 850 1600 370 370 370 400 400 400 400 400 400 600 650 700 800 1000)
for idx in "${!is[@]}"; do
   mA=${is[$idx]}
   mH=${js[$idx]}
   ks=0.03
   echo $wA
    echo "Copying mass (mA,mH)" # ($mA,$mH)
    newdir="$default"_mA"$mA"_mH"$mH"
    mkdir $newdir
    cp "$default"_customizecards.dat $newdir/"$default"_mA"$mA"_mH"$mH"_customizecards.dat
    cp "$default"_extramodels.dat $newdir/"$default"_mA"$mA"_mH"$mH"_extramodels.dat
    cp "$default"_madspin_card.dat $newdir/"$default"_mA"$mA"_mH"$mH"_madspin_card.dat
    cp "$default"_proc_card.dat $newdir/"$default"_mA"$mA"_mH"$mH"_proc_card.dat
    cp "$default"_run_card.dat $newdir/"$default"_mA"$mA"_mH"$mH"_run_card.dat
    # modify output name
    sed -i 's/'$default'/'$default'_mA'$mA'_mH'$mH'/g' $newdir/"$default"_mA"$mA"_mH"$mH"_proc_card.dat
    # Modify mass parameter
    sed -i 's/AMASS/'$mA'.0/g' $newdir/"$default"_mA"$mA"_mH"$mH"_customizecards.dat
    sed -i 's/HMASS/'$mH'.0/g' $newdir/"$default"_mA"$mA"_mH"$mH"_customizecards.dat
    sed -i 's/HWIDTH/'$(bc <<< "$ks * $mH")'/g' $newdir/"$default"_mA"$mA"_mH"$mH"_customizecards.dat
    sed -i 's/AWIDTH/'$(bc <<< "$ks * $mA")'/g' $newdir/"$default"_mA"$mA"_mH"$mH"_customizecards.dat
done


