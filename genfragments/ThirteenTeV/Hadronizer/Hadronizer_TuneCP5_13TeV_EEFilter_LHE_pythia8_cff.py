import FWCore.ParameterSet.Config as cms

from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.MCTunes2017.PythiaCP5Settings_cfi import *
from Configuration.Generator.PSweightsPythia.PythiaPSweightsSettings_cfi import *

generator = cms.EDFilter("Pythia8HadronizerFilter",
    maxEventsToPrint = cms.untracked.int32(1),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(13000.),
    PythiaParameters = cms.PSet(
        pythia8CommonSettingsBlock,
        pythia8CP5SettingsBlock,
        pythia8PSweightsSettingsBlock,
        parameterSets = cms.vstring('pythia8CommonSettings',
                                    'pythia8PSweightsSettings',
                                    'pythia8CP5Settings',
                                    )
    )
)

eeFilter = cms.EDFilter("LHEGenericFilter",
    src = cms.InputTag("source"),
    NumRequired = cms.int32(2),
    ParticleID = cms.vint32(11),
    AcceptLogic = cms.string("EQ") # LT meaning < NumRequired, GT >, EQ =, NE !=
)                   

ProductionFilterSequence = cms.Sequence(generator+eeFilter)
