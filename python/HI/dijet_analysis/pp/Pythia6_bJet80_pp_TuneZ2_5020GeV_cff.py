import FWCore.ParameterSet.Config as cms

from Configuration.Generator.PythiaUEZ2Settings_cfi import *

generator = cms.EDFilter("Pythia6GeneratorFilter",
                         comEnergy = cms.double(5020.0),
                         crossSection = cms.untracked.double(4.418e-04),
                         filterEfficiency = cms.untracked.double(8.640e-02),
                         maxEventsToPrint = cms.untracked.int32(-1),
                         pythiaHepMCVerbosity = cms.untracked.bool(False),
                         pythiaPylistVerbosity = cms.untracked.int32(False),
                         PythiaParameters = cms.PSet(pythiaUESettingsBlock,
                                                     processParameters = cms.vstring('MSEL=1   ! QCD hight pT processes',
                                                                                     'CKIN(3)= 80  ! minimum pt hat for hard interactions',
                                                                                     ),
                                                     parameterSets = cms.vstring('pythiaUESettings',
                                                                                 'processParameters',
                                                                                 )
                                                     )
                         )

bfilter = cms.EDFilter("MCSingleParticleFilter",
                       MaxEta     = cms.untracked.vdouble(2.5, 2.5),
                       MinEta     = cms.untracked.vdouble(-2.5, -2.5),
                       MinPt      = cms.untracked.vdouble(0.0, 0.0),
                       ParticleID = cms.untracked.vint32(5, -5)
                       )

ProductionFilterSequence = cms.Sequence(generator*bfilter)



