import FWCore.ParameterSet.Config as cms

from Configuration.Generator.PyquenDefaultSettings_cff import *
from Configuration.Generator.PythiaUEZ2Settings_cfi import *

hiSignal = cms.EDFilter("PyquenGeneratorFilter",
                        qgpParameters,
                        pyquenParameters,
                        aBeamTarget = cms.double(208.0),
                        comEnergy = cms.double(5020.0),
                        doQuench = cms.bool(False),
                        bFixed = cms.double(0.0), ## fixed impact param (fm); valid only if cflag_=0
                        cFlag = cms.int32(0), ## centrality flag
                        bMin = cms.double(0.0), ## min impact param (fm); valid only if cflag_!=0
                        bMax = cms.double(0.0), ## max impact param (fm); valid only if cflag_!=0

                        filterType     = cms.untracked.string('EcalGenEvtSelectorFrag'),
                        particlePt     = cms.vdouble(0),
                        particleStatus = cms.vint32(1),
                        particles      = cms.vint32(22),
                        etaMax         = cms.double(2.4),   # Photon eta cut
                        partonStatus = cms.vint32(2, 2, 2, 2, 2, 2, 2, 1),
                        partonPt = cms.vdouble(0,0,0,0,0,0,0,0),
                        partons = cms.vint32(1, 2, 3, 4, 5, 6, 21, 22), # parton cut is not functioning
                        maxTries = cms.untracked.int32(5000),

                        PythiaParameters = cms.PSet(
                            pythiaUESettingsBlock,
                            customProcesses = cms.vstring('MSEL=0   ! User processes'),
                            allQCDPhotonChannel = cms.vstring(    'MSUB(11)=1', # q+q->q+q
                                                                  'MSUB(12)=1', # q+qbar->q+qbar
                                                                  'MSUB(13)=1', # q+qbar->g+g
                                                                  'MSUB(28)=1', # q+g->q+g
                                                                  'MSUB(53)=1', # g+g->q+qbar
                                                                  'MSUB(68)=1', # g+g->g+g
                                                                  # Leading order photons
                                                                  'MSUB(14)=1', # q+qbar->g+gamma
                                                                  'MSUB(18)=1', # q+qbar->gamma+gamma
                                                                  'MSUB(29)=1', # q+g->q+gamma
                                                                  'MSUB(114)=1', # g+g->gamma+gamma
                                                                  'MSUB(115)=1' # g+g->g+gamma
                                                              ),
                            parameterSets = cms.vstring('pythiaUESettings',
                                                        'customProcesses',
                                                        'allQCDPhotonChannel',
                                                        'kinematics'),
                            kinematics = cms.vstring('CKIN(3)=10',
                                                     'CKIN(4)=9999'
                                                 )
                        )
)


hiSignal.embeddingMode = False

ProductionFilterSequence = cms.Sequence(hiSignal)
