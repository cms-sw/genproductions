#!/bin/sh
JOBS="jobs"
TEMP="templatecards"
PROC="ttbarDM__dilepton__DMsimp_LO_ps_spin0"

### Create cards for all mass points

for MCHI in 1 20 30 40 45 49 51 55; do
    if ((${MCHI}==1))
       then
	   for MPHI in 50 100 150 200 250 300 350 400 450 500; do
	       MODEL=${PROC}"__mchi_"${MCHI}"_mphi_"${MPHI}"_gSM_1_gDM_1_6800GeV"
	       mkdir -p "${JOBS}/${MODEL}"
	       cp ${TEMP}/${PROC}_run_card.dat "${JOBS}/${MODEL}/${MODEL}_run_card.dat"
	       cp ${TEMP}/${PROC}_madspin_card.dat "${JOBS}/${MODEL}/${MODEL}_madspin_card.dat"
	       cp ${TEMP}/${PROC}_extramodels.dat "${JOBS}/${MODEL}/${MODEL}_extramodels.dat"
	       sed "s/.*set param_card MASS  52.*/set param_card MASS  52 ${MCHI}/;s/.*set param_card MASS  54.*/set param_card MASS  54 ${MPHI}/" ${TEMP}/${PROC}_customizecards.dat > "${JOBS}/${MODEL}/${MODEL}_customizecards.dat"
	       sed "s/.*output.*/output ttbarDM__dilepton__DMsimp_LO_ps_spin0__mchi_${MCHI}_mphi_${MPHI}_gSM_1_gDM_1_6800GeV -nojpeg/" ${TEMP}/${PROC}_proc_card.dat > "${JOBS}/${MODEL}/${MODEL}_proc_card.dat"
	   done
    else
	MPHI=100
        MODEL=${PROC}"__mchi_"${MCHI}"_mphi_"${MPHI}"_gSM_1_gDM_1_6800GeV"
        mkdir -p "${JOBS}/${MODEL}"
        cp ${TEMP}/${PROC}_run_card.dat "${JOBS}/${MODEL}/${MODEL}_run_card.dat"
        cp ${TEMP}/${PROC}_madspin_card.dat "${JOBS}/${MODEL}/${MODEL}_madspin_card.dat"
        cp ${TEMP}/${PROC}_extramodels.dat "${JOBS}/${MODEL}/${MODEL}_extramodels.dat"
	sed "s/.*set param_card MASS  52.*/set param_card MASS  52 ${MCHI}/;s/.*set param_card MASS  54.*/set param_card MASS  54 ${MPHI}/" ${TEMP}/${PROC}_customizecards.dat > "${JOBS}/${MODEL}/${MODEL}_customizecards.dat"
	sed "s/.*output.*/output ttbarDM__dilepton__DMsimp_LO_ps_spin0__mchi_${MCHI}_mphi_${MPHI}_gSM_1_gDM_1_6800GeV -nojpeg/" ${TEMP}/${PROC}_proc_card.dat > "${JOBS}/${MODEL}/${MODEL}_proc_card.dat"
    fi
done
