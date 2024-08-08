#!/bin/bash

default=$(basename $(pwd))

skip_headers=1
while IFS=, read -r MA MH lambda2 lambda3
do
    if ((skip_headers))
    then
        ((skip_headers--))
    else
        echo "Creating cards for: MA=$MA MH=$MH"

        # specify the natural line width ks=width/mass
        ks=0.03

        newdir="$default"_MA-"$MA"_MH-"$MH"
        mkdir $newdir
        cp "$default"_customizecards.dat $newdir/"$default"_MA-"$MA"_MH-"$MH"_customizecards.dat
        cp "$default"_extramodels.dat $newdir/"$default"_MA-"$MA"_MH-"$MH"_extramodels.dat
        cp "$default"_madspin_card.dat $newdir/"$default"_MA-"$MA"_MH-"$MH"_madspin_card.dat
        cp "$default"_proc_card.dat $newdir/"$default"_MA-"$MA"_MH-"$MH"_proc_card.dat
        cp "$default"_run_card.dat $newdir/"$default"_MA-"$MA"_MH-"$MH"_run_card.dat
        # modify output name
        sed -i "s/"$default"/"$default"_MA-"$MA"_MH-"$MH"/g" $newdir/"$default"_MA-"$MA"_MH-"$MH"_proc_card.dat
        # Modify mass parameter
        sed -i "s/AMASS/"$MA".0/g" $newdir/"$default"_MA-"$MA"_MH-"$MH"_customizecards.dat
        sed -i "s/HMASS/"$MH".0/g" $newdir/"$default"_MA-"$MA"_MH-"$MH"_customizecards.dat
        sed -i "s/HWIDTH/"$(bc <<< "$ks * $MH")"/g" $newdir/"$default"_MA-"$MA"_MH-"$MH"_customizecards.dat
        sed -i "s/AWIDTH/"$(bc <<< "$ks * $MA")"/g" $newdir/"$default"_MA-"$MA"_MH-"$MH"_customizecards.dat
        sed -i "s/lambda2/"$lambda2"/g" $newdir/"$default"_MA-"$MA"_MH-"$MH"_customizecards.dat
        sed -i "s/lambda3/"$lambda3"/g" $newdir/"$default"_MA-"$MA"_MH-"$MH"_customizecards.dat

    fi
done < points.csv 
