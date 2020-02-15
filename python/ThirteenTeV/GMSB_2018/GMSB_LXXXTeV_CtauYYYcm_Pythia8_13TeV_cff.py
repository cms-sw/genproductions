import FWCore.ParameterSet.Config as cms

from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.FILESettings_cfi import *

generator = cms.EDFilter("Pythia8GeneratorFilter",
	comEnergy = cms.double(13000.0),
	pythiaHepMCVerbosity = cms.untracked.bool(False),
	pythiaPylistVerbosity = cms.untracked.int32(1),
        filterEfficiency = cms.untracked.double(1.0),
        SLHAFileForPythia8 = cms.string('Configuration/Generator/data/GMSB/GMSB_LambdaXXXTeV_CTauYYYcm.slha'),
                PythiaParameters = cms.PSet(
                pythia8CommonSettingsBlock, 
                pythia8CUEP8M1SettingsBlock, 
                processParameters = cms.vstring(
                    'ParticleDecays:limitTau0 = off',
                    'ParticleDecays:tau0Max = 10000000',
        	    'SUSY:all on',
                ),
                parameterSets = cms.vstring('pythia8CommonSettings',
                                            'pythia8TUNESettings',
                                            'processParameters')
   )
)

ProductionFilterSequence = cms.Sequence(generator)
