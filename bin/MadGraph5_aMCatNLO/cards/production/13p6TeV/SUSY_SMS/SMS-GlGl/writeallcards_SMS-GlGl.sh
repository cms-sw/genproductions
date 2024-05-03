#!/bin/sh
JOBS="jobs"
TEMP="templatecards"
PROC="SMS-GlGl"
PART="_mGl-"

### Create cards and SLHAs for all mass points

#for MGL in {600..750..50} {800..1600..25} {1650..2300..50}; do
for MGL in {600..2850..25}; do
    MODEL=${PROC}${PART}${MGL}
    mkdir -p "${JOBS}/${MODEL}"
    cp ${TEMP}/${PROC}_run_card.dat "${JOBS}/${MODEL}/${MODEL}_run_card.dat"
    sed "s/%MGL%/${MGL}/g" ${TEMP}/${PROC}_proc_card.dat > "${JOBS}/${MODEL}/${MODEL}_proc_card.dat"
    sed "s/%MGL%/${MGL}/g" ${TEMP}/${PROC}_customizecards.dat > "${JOBS}/${MODEL}/${MODEL}_customizecards.dat"
    sed "s/%MGL%/${MGL}/g" ${TEMP}/${PROC}.slha > ${JOBS}/${MODEL}/${MODEL}.slha
done
