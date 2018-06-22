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
                            'ExcitedFermion:bg2bStar = on',
                            '4000005:m0 = 4000.0',
                            '4000005:onMode = off',
                            '4000005:onIfMatch = 22 5',
                            'ExcitedFermion:Lambda = 4000.0',
                            'ExcitedFermion:coupFprime = 0.5',
                            'ExcitedFermion:coupF = 0.5',
                            'ExcitedFermion:coupFcol = 0.5'
                            ),
                        parameterSets = cms.vstring(
			    'pythia8CommonSettings',
                            'pythia8CP5Settings',
			    'processParameters',
			    )
                  )
)
ProductionFilterSequence = cms.Sequence(generator)


