import FWCore.ParameterSet.Config as cms

from Configuration.Generator.PythiaUEZ2Settings_cfi import *

generator = cms.EDFilter("Pythia6GeneratorFilter",
                         comEnergy = cms.double(2760.0),
                         filterEfficiency = cms.untracked.double(1.0000),
                         maxEventsToPrint = cms.untracked.int32(0),
                         pythiaHepMCVerbosity = cms.untracked.bool(False),
                         pythiaPylistVerbosity = cms.untracked.int32(0),

                         PythiaParameters = cms.PSet(
                             pythiaUESettingsBlock,
                             customProcesses = cms.vstring('MSEL=0   ! User processes'),
                             leadingOrderQCDPhotonChannel = cms.vstring(    # Leading order photons
                                 'MSUB(14)=1', # q+qbar->g+gamma
                                 'MSUB(18)=1', # q+qbar->gamma+gamma
                                 'MSUB(29)=1', # q+g->q+gamma
                                 'MSUB(114)=1', # g+g->gamma+gamma
                                 'MSUB(115)=1' # g+g->g+gamma
                             ),

                             parameterSets = cms.vstring('pythiaUESettings',
                                                         'customProcesses',
                                                         'leadingOrderQCDPhotonChannel',
                                                         'kinematics'),
                             kinematics = cms.vstring('CKIN(3)=15',
                                                      'CKIN(4)=9999',
                                                      "CKIN(7)=-3.",  #min rapidity
                                                      "CKIN(8)=3."    #max rapidity
                                                  )
                         )
)

ProductionFilterSequence = cms.Sequence(generator)
