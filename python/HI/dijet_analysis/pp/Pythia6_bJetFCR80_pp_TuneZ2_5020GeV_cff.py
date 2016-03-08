import FWCore.ParameterSet.Config as cms

from Configuration.Generator.PythiaUEZ2Settings_cfi import *

generator = cms.EDFilter("Pythia6GeneratorFilter",
                         comEnergy = cms.double(5020.0),
                         crossSection = cms.untracked.double(1.468e+03), #pb
                         filterEfficiency = cms.untracked.double(1.),
                         maxEventsToPrint = cms.untracked.int32(-1),
                         pythiaHepMCVerbosity = cms.untracked.bool(False),
                         pythiaPylistVerbosity = cms.untracked.int32(False),
                         PythiaParameters = cms.PSet(pythiaUESettingsBlock,
                                                     processParameters = cms.vstring('MSEL=5   ! b-quark flavor creation processes',
                                                                                     'CKIN(3)= 80  ! minimum pt hat for hard interactions',
                                                                                     ),
                                                     parameterSets = cms.vstring('pythiaUESettings',
                                                                                 'processParameters',
                                                                                 )
                                                     )
                         )

configurationMetadata = cms.untracked.PSet(
    annotation = cms.untracked.string('PYTHIA6, b-jets from flavor creation (MSEL=5), pt-hat > 80 GeV, at sqrt(s) = 5.02 TeV')
    )

ProductionFilterSequence = cms.Sequence(generator)



