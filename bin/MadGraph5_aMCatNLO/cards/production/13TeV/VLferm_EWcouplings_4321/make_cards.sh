#!/bin/bash

export PROC_NAME=${1}
export VLL_MASS=${2}

base_name="VLferm_EWcouplings_4321_m${VLL_MASS}GeV_${PROC_NAME}_to_4b"

if [ "${PROC_NAME}" == "EE" ]; then
    ./generate_EE_proc_card.sh ${base_name}
elif [ "${PROC_NAME}" == "EN" ]; then
    ./generate_EN_proc_card.sh ${base_name}
elif [ "${PROC_NAME}" == "NN" ]; then
    ./generate_NN_proc_card.sh ${base_name}
else
    echo "Unrecognized Process Name ${PROC_NAME}, must be one of 'EE', 'EN', 'NN'"
fi

./generate_customizecards.sh ${VLL_MASS} ${base_name}

cp default_run_card.dat ${base_name}_run_card.dat
cp default_extramodels.dat ${base_name}_extramodels.dat
