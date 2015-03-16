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
                            'ExcitedFermion:ug2uStar = on',
                            'ExcitedFermion:dg2dStar = on',
                            '4000001:m0 = 7000.',
                            '4000001:onMode = off',
                            '4000001:onIfMatch = 22 1',
                            '4000002:m0 = 7000.',
                            '4000002:onMode = off',
                            '4000002:onIfMatch = 22 2',
                            'ExcitedFermion:Lambda = 7000.',
                            'ExcitedFermion:coupFprime = 0.5',
                            'ExcitedFermion:coupF = 0.5',
                            'ExcitedFermion:coupFcol = 0.5'
                            ),
                        parameterSets = cms.vstring(
			    'pythia8CommonSettings',
                            'pythia8CUEP8M1Settings',
			    'processParameters',
			    )
                  )
)
ProductionFilterSequence = cms.Sequence(generator)
