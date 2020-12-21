# Disappearing Tracks AMSB
https://indico.cern.ch/event/725187/contributions/3018385/attachments/1656960/2652924/disappTrksMCRequest.pdf

    cd $CMSSW_BASE/src/Configuration/GenProduction/python/ThirteenTeV/DisappTrksAMSB/
    ./createPoints.py
    cd $CMSSW_BASE/src
    scramv1 b -j 9

94X e.g.:

    cmsDriver.py Configuration/GenProduction/python/ThirteenTeV/DisappTrksAMSB/test/AMSB_chargino_M-700GeV_CTau-100cm_TuneCP5_13TeV_pythia8_cff.py \
    --fileout file:AMSB_chargino700GeV_ctau100cm_step1.root \
    --mc \
    --eventcontent RAWSIM \
    --customise Configuration/DataProcessing/Utils.addMonitoring,SimG4Core/CustomPhysics/Exotica_HSCP_SIM_cfi,SimG4Core/Application/customiseSequentialSim.customiseSequentialSim \
    --datatier GEN-SIM \
    --conditions 94X_mc2017_realistic_v10 \
    --beamspot Realistic25ns13TeVEarly2017Collision \
    --step GEN,SIM \
    --geometry DB:Extended \
    --era Run2_2017 \
    --python_filename AMSB_chargino700GeV_ctau100cm_step1.py \
    --no_exec \
    -n 10 
