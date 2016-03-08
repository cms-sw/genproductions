import FWCore.ParameterSet.Config as cms
from Configuration.Generator.PythiaUEZ2Settings_cfi import *

generator = cms.EDFilter("Pythia6GeneratorFilter",
                         comEnergy = cms.double(5020.0),
                         crossSection = cms.untracked.double(6.159e-05),  
                         filterEfficiency = cms.untracked.double(1.),
                         maxEventsToPrint = cms.untracked.int32(1),
                         pythiaHepMCVerbosity = cms.untracked.bool(False),
                         pythiaPylistVerbosity = cms.untracked.int32(False),
                         PythiaParameters = cms.PSet(pythiaUESettingsBlock,
                                                     processParameters = cms.vstring('MSEL=1   ! Standard QCD processes',
                                                                                     'CKIN(3)= 120  ! minimum pt hat for hard interactions',
                                                                                     ),
                                                     parameterSets = cms.vstring('pythiaUESettings',
                                                                                 'processParameters',
                                                                                 )
                                                     )
                         )

configurationMetadata = cms.untracked.PSet(
    annotation = cms.untracked.string('PYTHIA6 QCD, pt-hat > 120 GeV, at sqrt(s) = 5.02 TeV')
    )

ProductionFilterSequence = cms.Sequence(generator)

