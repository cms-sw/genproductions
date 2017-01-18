import FWCore.ParameterSet.Config as cms

from Configuration.Generator.PythiaUEZ2Settings_cfi import *

generator = cms.EDFilter("Pythia6GeneratorFilter",
                         comEnergy = cms.double(2760.0),
                         crossSection = cms.untracked.double(9.913e-05),
                         filterEfficiency = cms.untracked.double(1.44e-01),
                         maxEventsToPrint = cms.untracked.int32(-1),
                         pythiaHepMCVerbosity = cms.untracked.bool(False),
                         pythiaPylistVerbosity = cms.untracked.int32(False),
                         PythiaParameters = cms.PSet(pythiaUESettingsBlock,
                                                     processParameters = cms.vstring('MSEL=1   ! QCD hight pT processes',
                                                                                     'CKIN(3)= 80  ! minimum pt hat for hard interactions',
                                                                                     "CKIN(7)=-3.",  #min rapidity
                                                                                     "CKIN(8)=3.",    #max rapidity
                                                                                     ),
                                                     parameterSets = cms.vstring('pythiaUESettings',
                                                                                 'processParameters',
                                                                                 )
                                                     )
                         )

bfilter = cms.EDFilter("MCSingleParticleFilter",
                       MaxEta     = cms.untracked.vdouble(3.0, 3.0),
                       MinEta     = cms.untracked.vdouble(-3.0, -3.0),
                       MinPt      = cms.untracked.vdouble(10.0, 10.0),
                       ParticleID = cms.untracked.vint32(4, -4)
                       )

ProductionFilterSequence = cms.Sequence(generator*bfilter)



