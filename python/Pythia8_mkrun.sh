#!/bin/bash

cd $CMSSW_BASE/src/Configuration/GenProduction/python/

scramv1 b --reset

for k in 15_20 20_25 25_30 30_35 35 
 


do echo `ls PYTHIA8_PhotonJetpt${k}_10TeV_cff.py`


cmsDriver.py Configuration/GenProduction/python/PYTHIA8_PhotonJetpt${k}_10TeV_cff.py -s GEN --eventcontent RAWSIM --datatier GEN --conditions FrontierConditions_GlobalTag,IDEAL_V9::All -n 100 --no_exec
        #mv PYTHIA8_PhotonJetpt${k}_10TeV_cff_py_GEN_IDEAL_V9.py PYTHIA8_PhotonJetpt${k}_10TeV_cff_py_GEN_IDEAL.py
        echo "process.genParticles.abortOnUnknownPDGCode = False" >> PYTHIA8_PhotonJetpt${k}_10TeV_cff_py_GEN_IDEAL.py

cmsDriver.py Configuration/GenProduction/python/PYTHIA8_PhotonJetpt${k}_10TeV_cff.py -s GEN,SIM,DIGI,L1,DIGI2RAW,HLT --eventcontent RAWSIM --datatier GEN-SIM-RAW --conditions FrontierConditions_GlobalTag,IDEAL_V9::All -n 100 --no_exec
        #mv PYTHIA8_PhotonJetpt${k}_10TeV_cff_py_GEN_SIM_DIGI_L1_DIGI2RAW_HLT_IDEAL_V9.py PYTHIA8_PhotonJetpt${k}_10TeV_cff_py_GEN_SIM_DIGI_L1_DIGI2RAW_HLT_IDEAL.py
        echo "process.genParticles.abortOnUnknownPDGCode = False" >> PYTHIA8_PhotonJetpt${k}_10TeV_cff_py_GEN_SIM_DIGI_L1_DIGI2RAW_HLT_IDEAL.py

done
