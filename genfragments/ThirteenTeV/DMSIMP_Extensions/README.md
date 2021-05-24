# Long-Lived Extensions for Mono Z Dark Matter Models

    cd $CMSSW_BASE/src/Configuration/GenProduction/python/ThirteenTeV/DMSIMP_Extensions/
    python createFragments.py
    cd $CMSSW_BASE/src
    scramv1 b -j 9

94X e.g.:

    cmsDriver.py Configuration/GenProduction/python/ThirteenTeV/DMSIMP_Extensions/DMSimp_DM_ZLL_LO_Vector_GQ0p25_GDM1p0_MY1-500_MXd-150/MX1-1_ctau0p5.py \
    --fileout file:DMSimp_DM_ZLL_LO_Vector_GQ0p25_GDM1p0_MY1-500_MXd-150/DMSimp_DM_ZLL_LO_Vector_GQ0p25_GDM1p0_MY1-500_MX2-150_MX1-1_ctau-0p5.root \
    --mc \
    --eventcontent RAWSIM,LHE \
    --datatier GEN-SIM,LHE  \
    --conditions auto:mc  \
    --python_filename DMSimp_DM_ZLL_LO_Vector_GQ0p25_GDM1p0_MY1-500_MXd-150/MX1-1_ctau-0p5_py_GEN.py \
    --step LHE,GEN  \
    -n 50000  \
    -o 50000

