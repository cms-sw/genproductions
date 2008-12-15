import FWCore.ParameterSet.Config as cms

source = cms.Source("Herwig6Source",
    HerwigParameters = cms.PSet(
        herwigQCDjets = cms.vstring(
            'IPROC      = 1500       ! QCD 2->2 processes', 
            'PTMIN      = 1400.       ! minimum pt in hadronic jet',
            'MODPDF(1)  = 10041      ! PDF set according to LHAGLUE', 
            'MODPDF(2)  = 10041      ! CTEQ6L', 
            'JMUEO      = 1          ! multiparton interaction model',
            'PTJIM      = 4.449      ! 2.8x(sqrt(s)/1.8TeV)^0.27 @ 10 TeV',
            'JMRAD(73)  = 1.8        ! inverse proton radius squared',
            'PRSOF      = 0.0        ! prob. of a soft underlying event',
            'MAXER      = 1000000    ! max error'),
        parameterSets = cms.vstring('herwigQCDjets')
    ),
    doMPInteraction = cms.untracked.bool(True),
    useJimmy = cms.untracked.bool(True),
    herwigHepMCVerbosity = cms.untracked.bool(False),
    herwigVerbosity = cms.untracked.int32(0),
    lhapdfSetPath = cms.untracked.string('/afs/cern.ch/sw/lcg/external/MCGenerators/lhapdf/5.2.3/share/PDFsets'),
    comEnergy = cms.untracked.double(10000.0),
    crossSection = cms.untracked.double(0.1652),
    filterEfficiency = cms.untracked.double(1.0),
    printCards = cms.untracked.bool(False),
    maxEventsToPrint = cms.untracked.int32(0)
)
# Don't forget this!
#abortOnUnknownPDGCode = cms.untracked.bool(False)

emptyEventFilter = cms.EDFilter("Herwig6Filter")
ProductionFilterSequence = cms.Sequence(emptyEventFilter)
