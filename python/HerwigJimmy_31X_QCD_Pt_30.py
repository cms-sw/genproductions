import FWCore.ParameterSet.Config as cms


source = cms.Source("EmptySource")

generator = cms.EDFilter("Herwig6GeneratorFilter",
    HerwigParameters = cms.PSet(
        herwigQCDjets = cms.vstring(
            'IPROC      = 1500       ! QCD 2->2 processes', 
            'PTMIN      = 30.       ! minimum pt in hadronic jet',
            'MODPDF(1)  = 10041      ! PDF set according to LHAGLUE', 
            'MODPDF(2)  = 10041      ! CTEQ6L', 
            'JMUEO      = 1          ! multiparton interaction model',
            'PTJIM      = 4.449      ! 2.8x(sqrt(s)/1.8TeV)^0.27 @ 10 TeV',
            'JMRAD(73)  = 1.8        ! inverse proton radius squared',
            'PRSOF      = 0.0        ! prob. of a soft underlying event',
            'MAXER      = 1000000    ! max error'),
        parameterSets = cms.vstring('herwigQCDjets')
    ),
    doMPInteraction = cms.bool(True),
    useJimmy = cms.bool(True),
    herwigHepMCVerbosity = cms.untracked.bool(False),
    herwigVerbosity = cms.untracked.int32(0),
    lhapdfSetPath = cms.untracked.string(''),
    comEnergy = cms.double(10000.0),
    crossSection = cms.untracked.double(8.8983e07),
    filterEfficiency = cms.untracked.double(1.0),
    printCards = cms.untracked.bool(False),
    maxEventsToPrint = cms.untracked.int32(0),
    emulatePythiaStatusCodes = cms.untracked.bool(False)
)

ProductionFilterSequence = cms.Sequence(generator)
