#!/bin/sh

cd $CMSSW_BASE/src/Configuration/GenProduction/python/

cat Herwigpp_minkttable.txt | while read MINKT XS EVTS; do
	sed -e "s/__MINKT__/$MINKT/;s/__XS__/$XS/" Herwigpp_base_cff.py > Herwigpp_QCD_Pt_$MINKT.py;
done

scramv1 b --reset

cat Herwigpp_minkttable.txt | while read MINKT XS EVTS; do
	cmsDriver.py Configuration/GenProduction/python/Herwigpp_QCD_Pt_$MINKT.py -s GEN --eventcontent RAWSIM --datatier GEN --conditions FrontierConditions_GlobalTag,IDEAL_V6::All -n $EVTS --no_exec
	echo "process.genParticles.abortOnUnknownPDGCode = False" >> Herwigpp_QCD_Pt_${MINKT}_py_GEN_IDEAL.py

	cmsDriver.py Configuration/GenProduction/python/Herwigpp_QCD_Pt_$MINKT.py -s GEN,SIM,DIGI,L1,DIGI2RAW,HLT --eventcontent RAWSIM --datatier GEN-SIM-RAW --conditions FrontierConditions_GlobalTag,IDEAL_V6::All -n $EVTS --no_exec
	echo "process.genParticles.abortOnUnknownPDGCode = False" >> Herwigpp_QCD_Pt_${MINKT}_py_GEN_SIM_DIGI_L1_DIGI2RAW_HLT_IDEAL.py
done
