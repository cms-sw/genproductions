import FWCore.ParameterSet.Config as cms

from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.Pythia8CUEP8M1Settings_cfi import *

generator = cms.EDFilter("Pythia8GeneratorFilter",
                  comEnergy = cms.double(13000.0),
                  crossSection = cms.untracked.double(1.0),
                  filterEfficiency = cms.untracked.double(1.0),
                  maxEventsToPrint = cms.untracked.int32(0),
                  pythiaHepMCVerbosity = cms.untracked.bool(False),
                  pythiaPylistVerbosity = cms.untracked.int32(0),
                  PythiaParameters = cms.PSet(
		        pythia8CommonSettingsBlock,
			pythia8CUEP8M1SettingsBlock,
                        processParameters = cms.vstring(
                            'ExcitedFermion:bg2bStar = on',
                            '4000005:m0 = 1000.',
                            '4000005:onMode = off',
                            '4000005:onIfMatch = 22 5',
                            'ExcitedFermion:Lambda = 1000.',
                            'ExcitedFermion:coupFprime = 1.0',
                            'ExcitedFermion:coupF = 1.0',
                            'ExcitedFermion:coupFcol = 1.0'
                            ),
                        parameterSets = cms.vstring(
			    'pythia8CommonSettings',
                            'pythia8CUEP8M1Settings',
			    'processParameters',
			    )
                  )
)
ProductionFilterSequence = cms.Sequence(generator)
