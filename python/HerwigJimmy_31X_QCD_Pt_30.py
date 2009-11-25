import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.HerwigUESettings_cfi import *

source = cms.Source("EmptySource")

generator = cms.EDFilter("Herwig6GeneratorFilter",
    HerwigParameters = cms.PSet(
        herwigUESettingsBlock,
        herwigQCDjets = cms.vstring(
            'IPROC      = 1500       ! QCD 2->2 processes', 
            'PTMIN      = 30.       ! minimum pt in hadronic jet'),
            parameterSets = cms.vstring(
                'herwigUESettings',
		'herwigQCDjets')
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
