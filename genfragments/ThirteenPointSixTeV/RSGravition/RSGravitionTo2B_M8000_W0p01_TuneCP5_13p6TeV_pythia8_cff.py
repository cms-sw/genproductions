import FWCore.ParameterSet.Config as cms

from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.MCTunesRun3ECM13p6TeV.PythiaCP5Settings_cfi import *
from Configuration.Generator.PSweightsPythia.PythiaPSweightsSettings_cfi import *


generator = cms.EDFilter("Pythia8ConcurrentGeneratorFilter",
	comEnergy = cms.double(13600.0),
	#crossSection = cms.untracked.double(22.32),
	filterEfficiency = cms.untracked.double(1),
	maxEventsToPrint = cms.untracked.int32(0),
	pythiaHepMCVerbosity = cms.untracked.bool(False),
	pythiaPylistVerbosity = cms.untracked.int32(1),
	PythiaParameters = cms.PSet(
                pythia8CommonSettingsBlock,
                pythia8CP5SettingsBlock,
            pythia8PSweightsSettingsBlock,
		processParameters = cms.vstring(
                        'ExtraDimensionsG*:ffbar2G* = on', 
			'ExtraDimensionsG*:kappaMG = 0.45522024600000005',
			'5100039:m0 = 8000',
			'5100039:onMode = off',
			'5100039:onIfAny = 5'
		),
                parameterSets = cms.vstring('pythia8CommonSettings',
                                            'pythia8CP5Settings',
                                            'pythia8PSweightsSettings',
                                            'processParameters',
                                            )
	)
)

ProductionFilterSequence = cms.Sequence(generator)
