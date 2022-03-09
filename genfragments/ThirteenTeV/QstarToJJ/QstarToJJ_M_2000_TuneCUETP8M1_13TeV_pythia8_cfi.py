import FWCore.ParameterSet.Config as cms

from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.Pythia8CUEP8M1Settings_cfi import *

generator = cms.EDFilter("Pythia8GeneratorFilter",
	comEnergy = cms.double(13000.0),
	crossSection = cms.untracked.double(1),
	filterEfficiency = cms.untracked.double(1),
	maxEventsToPrint = cms.untracked.int32(0),
	pythiaHepMCVerbosity = cms.untracked.bool(False),
	pythiaPylistVerbosity = cms.untracked.int32(1),
	PythiaParameters = cms.PSet(
                pythia8CommonSettingsBlock,
                pythia8CUEP8M1SettingsBlock,
		processParameters = cms.vstring(
                        'ExcitedFermion:ug2uStar = on', 
                        'ExcitedFermion:dg2dStar = on', 
                        '4000001:m0 = 2000.', 
                        '4000001:onMode = off', 
                        '4000001:onIfMatch = 21 1', 
                        '4000002:m0 = 2000.', 
                        '4000002:onMode = off', 
                        '4000002:onIfMatch = 21 2', 
                        'ExcitedFermion:Lambda = 2000.', 
                        'ExcitedFermion:coupFprime = 1.', 
                        'ExcitedFermion:coupF = 1.', 
                        'ExcitedFermion:coupFcol = 1.'
		),
                parameterSets = cms.vstring('pythia8CommonSettings',
                                            'pythia8CUEP8M1Settings',
                                            'processParameters',
                                            )
	)
)

ProductionFilterSequence = cms.Sequence(generator)
