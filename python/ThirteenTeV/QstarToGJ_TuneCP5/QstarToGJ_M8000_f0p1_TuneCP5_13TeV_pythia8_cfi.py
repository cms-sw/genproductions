import FWCore.ParameterSet.Config as cms

from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.MCTunes2017.PythiaCP5Settings_cfi import *

generator = cms.EDFilter("Pythia8GeneratorFilter",
                  comEnergy = cms.double(13000.0),
                  crossSection = cms.untracked.double(1.0),
                  filterEfficiency = cms.untracked.double(1.0),
                  maxEventsToPrint = cms.untracked.int32(0),
                  pythiaHepMCVerbosity = cms.untracked.bool(False),
                  pythiaPylistVerbosity = cms.untracked.int32(0),
                  PythiaParameters = cms.PSet(
		        pythia8CommonSettingsBlock,
                        pythia8CP5SettingsBlock,
                        processParameters = cms.vstring(
                            'ExcitedFermion:ug2uStar = on',
                            'ExcitedFermion:dg2dStar = on',
                            '4000001:m0 = 8000.0',
                            '4000001:onMode = off',
                            '4000001:onIfMatch = 22 1',
                            '4000002:m0 = 8000.0',
                            '4000002:onMode = off',
                            '4000002:onIfMatch = 22 2',
                            'ExcitedFermion:Lambda = 8000.0',
                            'ExcitedFermion:coupFprime = 0.1',
                            'ExcitedFermion:coupF = 0.1',
                            'ExcitedFermion:coupFcol = 0.1'
                            ),
                        parameterSets = cms.vstring(
			    'pythia8CommonSettings',
                            'pythia8CP5Settings',
			    'processParameters',
			    )
                  )
)
ProductionFilterSequence = cms.Sequence(generator)


