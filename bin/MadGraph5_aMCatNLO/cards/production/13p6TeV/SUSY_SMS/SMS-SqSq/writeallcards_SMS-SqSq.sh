#!/bin/sh
JOBS="jobs"
TEMP="templatecards"
PROC="SMS-SqSq"
PART="_mSq-"

### Create cards and SLHAs for all mass points

#for MPROD in {300..1800..50}; do
for MPROD in {300..2600..50}; do
    MODEL=${PROC}${PART}${MPROD}
    mkdir -p "${JOBS}/${MODEL}"
    cp ${TEMP}/${PROC}_run_card.dat "${JOBS}/${MODEL}/${MODEL}_run_card.dat"
    sed "s/%MPROD%/${MPROD}/g" ${TEMP}/${PROC}_proc_card.dat > "${JOBS}/${MODEL}/${MODEL}_proc_card.dat"
    sed "s/%MPROD%/${MPROD}/g" ${TEMP}/${PROC}_customizecards.dat > "${JOBS}/${MODEL}/${MODEL}_customizecards.dat"
    sed "s/%MPROD%/${MPROD}/g" ${TEMP}/${PROC}.slha > ${JOBS}/${MODEL}/${MODEL}.slha
done
