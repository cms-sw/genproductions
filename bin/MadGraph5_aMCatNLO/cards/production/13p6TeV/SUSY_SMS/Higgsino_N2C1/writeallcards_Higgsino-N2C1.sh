#!/bin/sh
JOBS="jobs"
TEMP="templatecards"
PROC="Higgsino-N2C1"
PARTN2="_mN2-"
PARTC1="_mC1-"

### Create cards and SLHAs for all mass points

for MN2 in 100 125 150 175 200 225 250 275 300 325 350 375 400 425 450 475 500 525 550 575 600; do
    for DM in 0.6 0.8 1 1.5 2 3 5 7.5 10 15 20 30 40 50; do
	MN1=`awk "BEGIN {printf \"%.2f\n\", (${MN2}-${DM})}"`	
	MC1=`awk "BEGIN {printf \"%.2f\n\", ((${MN1}+${MN2})/2)}"`
	MN2STR=${MN2/./p}
	MN1STR=${MN1/./p}
	MC1STR=${MC1/./p}
	MODEL=${PROC}${PARTN2}${MN2STR}${PARTC1}${MC1STR}
	mkdir -p "${JOBS}/${MODEL}"
	cp ${TEMP}/${PROC}_run_card.dat "${JOBS}/${MODEL}/${MODEL}_run_card.dat"
	sed "s/%MN2%/${MN2STR}/g;s/%MC1%/${MC1STR}/g" ${TEMP}/${PROC}_proc_card.dat > "${JOBS}/${MODEL}/${MODEL}_proc_card.dat"
	sed "s/%MN2%/${MN2}/g;s/%MN1%/${MN1}/g;s/%MC1%/${MC1}/g" ${TEMP}/${PROC}_customizecards.dat > "${JOBS}/${MODEL}/${MODEL}_customizecards.dat"
	sed "s/%MN2%/${MN2}/g;s/%MN1%/${MN1}/g;s/%MC1%/${MC1}/g" ${TEMP}/${PROC}.slha > ${JOBS}/${MODEL}/${MODEL}.slha
    done
done
