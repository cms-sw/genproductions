#!/bin/bash

CARDSDIR=${1}
WORKDIR=${2}


cd ${CARDSDIR}/DR_files
for dir in */; do
    for file in $dir/*; do
        cp $file ${WORKDIR}/tWa_NLO_DR1/SubProcesses/$dir
        cp $file ${WORKDIR}/processtmp/SubProcesses/$dir
    done
done
cd - 