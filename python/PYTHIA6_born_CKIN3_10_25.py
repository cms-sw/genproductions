import FWCore.ParameterSet.Config as cms

source = cms.Source("EmptySource")


from Configuration.Generator.PythiaUESettings_cfi import *

generator = cms.EDFilter("Pythia6GeneratorFilter",
                             pythiaHepMCVerbosity = cms.untracked.bool(False),
                             maxEventsToPrint = cms.untracked.int32(0),
                             pythiaPylistVerbosity = cms.untracked.int32(1),
                             filterEfficiency = cms.untracked.double(1.0),
                             comEnergy = cms.double(7000.0),
                             crossSection = cms.untracked.double(0.004285),
                             PythiaParameters = cms.PSet(
            pythiaUESettingsBlock,
                    processParameters = cms.vstring(
                    'MSEL=0 ',
                    'MSUB(18)=1       ',
                    'CKIN(3)=10.          ! minimum pt hat for hard interactions',
                    'CKIN(4)=25.          ! maximum pt hat for hard interactions'),
                    parameterSets = cms.vstring('pythiaUESettings',
                                   'processParameters')
                )
                         )

ProductionFilterSequence = cms.Sequence(generator)


