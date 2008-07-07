#!/bin/sh

cd $CMSSW_BASE/src/Configuration/GenProduction/python/

cat Herwig_minkttable.txt | while read MINKT XS EVTS; do
	sed -e "s/__MINKT__/$MINKT/;s/__XS__/$XS/" Herwigpp_base_cff.py > Herwig_QCD_Pt_$MINKT.py;
done

scramv1 b --reset

cat Herwig_minkttable.txt | while read MINKT XS EVTS; do
	cmsDriver.py Configuration/GenProduction/python/Herwig_QCD_Pt_$MINKT.py -s GEN --eventcontent RAWSIM --datatier GEN --conditions FrontierConditions_GlobalTag,STARTUP_V1::All -n $EVTS --no_exec
	echo "process.genParticles.abortOnUnknownPDGCode = False" >> Herwig_QCD_Pt_${MINKT}_py__GEN.py

	cmsDriver.py Configuration/GenProduction/python/Herwig_QCD_Pt_$MINKT.py -s GEN,SIM,DIGI,L1,DIGI2RAW,HLT --eventcontent RAWSIM --datatier GEN-SIM-RAW --conditions FrontierConditions_GlobalTag,STARTUP_V1::All -n $EVTS --no_exec
	echo "process.genParticles.abortOnUnknownPDGCode = False" >> Herwig_QCD_Pt_${MINKT}_py__GEN_SIM_DIGI_L1_DIGI2RAW_HLT.py
done
