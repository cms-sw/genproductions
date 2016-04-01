import FWCore.ParameterSet.Config as cms

from Configuration.Generator.PythiaUESettings_cfi import *

generator = cms.EDFilter("Pythia6GeneratorFilter",
                pythiaHepMCVerbosity = cms.untracked.bool(False),
                maxEventsToPrint = cms.untracked.int32(0),
                pythiaPylistVerbosity = cms.untracked.int32(0),
                filterEfficiency = cms.untracked.double(1.0),
                crossSection = cms.untracked.double(305.2),
                comEnergy = cms.double(5020.0),
                PythiaParameters = cms.PSet(pythiaUESettingsBlock,
                                            pythiaZeejets = cms.vstring('MSEL = 0 ! users defined processes only',
                                                                          'MSUB(15)=1  !qq->Z0/gamma*+g',
                                                                          'MSUB(30)=1  !qg->Z0/gamma*+q',
                                                                          'MDME( 174,1) = 0    !Z decay into d dbar', 
                                                                          'MDME( 175,1) = 0    !Z decay into u ubar', 
                                                                          'MDME( 176,1) = 0    !Z decay into s sbar', 
                                                                          'MDME( 177,1) = 0    !Z decay into c cbar', 
                                                                          'MDME( 178,1) = 0    !Z decay into b bbar', 
                                                                          'MDME( 179,1) = 0    !Z decay into t tbar', 
                                                                          'MDME( 182,1) = 1    !Z decay into e- e+', 
                                                                          'MDME( 183,1) = 0    !Z decay into nu_e nu_ebar', 
                                                                          'MDME( 184,1) = 0    !Z decay into mu- mu+', 
                                                                          'MDME( 185,1) = 0    !Z decay into nu_mu nu_mubar', 
                                                                          'MDME( 186,1) = 0    !Z decay into tau- tau+', 
                                                                          'MDME( 187,1) = 0    !Z decay into nu_tau nu_taubar', 
                                                                        ),
                                            parameterSets = cms.vstring('pythiaUESettings',
                                                                        'pythiaZeejets'),
                                            )
                        )

eegenfilter = cms.EDFilter("MCParticlePairFilter",
    Status         = cms.untracked.vint32(1, 1),
    MinPt          = cms.untracked.vdouble(10, 10),
    MaxEta         = cms.untracked.vdouble(2.5, 2.5),
    MinEta         = cms.untracked.vdouble(-2.5, -2.5),
    MinInvMass     = cms.untracked.double(60.0),
    MaxInvMass     = cms.untracked.double(120.0),
    ParticleCharge = cms.untracked.int32(-1),
    ParticleID1    = cms.untracked.vint32(11),
    ParticleID2    = cms.untracked.vint32(11)
)

ProductionFilterSequence = cms.Sequence(generator*eegenfilter)
