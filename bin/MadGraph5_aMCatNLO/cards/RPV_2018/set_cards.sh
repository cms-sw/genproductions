#!/bin/bash

# Define of Smuon Mass (1)  and Neutralino Mass (2)
list1=(200 250 300 350 400 450 500)
list2=(180 200 230 250 280 300 330 350 380 400 430 450 480)  

echo "Pull Request : Add DisplacedTop cards"

# Loop over both lists
for smuon in "${list1[@]}"; do
    for neu in "${list2[@]}"; do
        # Check if neutralino mass is smaller than smuon mass (with specific values)
        dm=$((smuon - neu))
        if [[ $neu -lt $smuon ]] &&  [[ !(($dm -eq 70 && $neu -ne 180) || \
            ($dm -eq 120 && $neu -ne 180) || \
            ($dm -eq 170 && $neu -ne 180) || \
            ($dm -eq 220 && $neu -ne 180) || \
            ($dm -eq 270 && $neu -ne 180)) ]]; then

            # Copy the reference param_card to create the one of the new sample
            # cp ./RPV_PROD_smuXXX_neuYYY_param_card.dat ./RPV_PROD_smu${smuon}_neu${neu}_param_card.dat
            cp ./RPV_PROD_smuXXX_neuYYY_run_card.dat ./RPV_PROD_smu${smuon}_neu${neu}_run_card.dat
            cp ./RPV_PROD_smuXXX_neuYYY_proc_card.dat ./RPV_PROD_smu${smuon}_neu${neu}_proc_card.dat
            cp ./RPV_PROD_smuXXX_neuYYY_customizecards.dat ./RPV_PROD_smu${smuon}_neu${neu}_customizecards.dat
            cp ./RPV_PROD_smuXXX_neuYYY_extramodels.dat ./RPV_PROD_smu${smuon}_neu${neu}_extramodels.dat

            # Edit the param_card.dat based on smuon and neutralino masses

            sed -i "s/XXX/${smuon}/g" ./RPV_PROD_smu${smuon}_neu${neu}_customizecards.dat
            sed -i "s/YYY/${neu}/g" ./RPV_PROD_smu${smuon}_neu${neu}_customizecards.dat
            
            # sed -i "s/XXX/${smuon}/g" ./RPV_PROD_smu${smuon}_neu${neu}_param_card.dat
            # sed -i "s/YYY/${neu}/g" ./RPV_PROD_smu${smuon}_neu${neu}_param_card.dat

            echo "Created and modified files for smuon mass $smuon and neutralino mass $neu"
        fi
    done
done
