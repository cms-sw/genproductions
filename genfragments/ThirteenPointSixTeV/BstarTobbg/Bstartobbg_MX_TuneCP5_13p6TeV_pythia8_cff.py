import FWCore.ParameterSet.Config as cms

from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.PSweightsPythia.PythiaPSweightsSettings_cfi import *
from Configuration.Generator.MCTunesRun3ECM13p6TeV.PythiaCP5Settings_cfi import *

generator = cms.EDFilter("Pythia8ConcurrentGeneratorFilter",
	comEnergy = cms.double(13600.0),
	#crossSection = cms.untracked.double(0.0000415),
	filterEfficiency = cms.untracked.double(1),
	maxEventsToPrint = cms.untracked.int32(0),
	pythiaHepMCVerbosity = cms.untracked.bool(False),
	pythiaPylistVerbosity = cms.untracked.int32(1),
	PythiaParameters = cms.PSet(
                pythia8CommonSettingsBlock,
                pythia8CP5SettingsBlock,
            pythia8PSweightsSettingsBlock,
		processParameters = cms.vstring('ExcitedFermion:qq2bStarq = on',
                '4000005:m0 = X',
                '4000005:onMode = off',
                '4000005:onIfMatch = 21 5',
                'ExcitedFermion:Lambda = X',
                'ExcitedFermion:coupFprime = 1.',
                'ExcitedFermion:coupF = 1.',
                'ExcitedFermion:coupFcol = 1.'
		),
                parameterSets = cms.vstring('pythia8CommonSettings',
                                            'pythia8CP5Settings',
                                            'pythia8PSweightsSettings',
                                            'processParameters',
                                            )
	)
)

ProductionFilterSequence = cms.Sequence(generator)
