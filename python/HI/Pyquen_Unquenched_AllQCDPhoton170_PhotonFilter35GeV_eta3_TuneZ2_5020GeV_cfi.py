import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.HI.PyquenTuneZ2Settings_cff import *

hiSignal = cms.EDFilter("PyquenGeneratorFilter",
                        collisionParameters5020GeV,
                        qgpParameters,
                        pyquenParameters,
                        doQuench = cms.bool(False),
                        bFixed = cms.double(0.0), ## fixed impact param (fm); valid only if cflag_=0
                        cFlag = cms.int32(0), ## centrality flag
                        bMin = cms.double(0.0), ## min impact param (fm); valid only if cflag_!=0
                        bMax = cms.double(0.0), ## max impact param (fm); valid only if cflag_!=0
                        
                        filterType     = cms.untracked.string('EcalGenEvtSelectorFrag'),
                        particlePt     = cms.vdouble(35),
                        particleStatus = cms.vint32(1),
                        particles      = cms.vint32(22),
                        etaMax         = cms.double(3),   # Photon eta cut
                        partonStatus = cms.vint32(2, 2, 2, 2, 2, 2, 2, 1),
                        partonPt = cms.vdouble(0,0,0,0,0,0,0,0),
                        partons = cms.vint32(1, 2, 3, 4, 5, 6, 21, 22), # parton cut is not functioning 
                        maxTries = cms.untracked.int32(5000),
                        protonSide = cms.untracked.int32(2), #pPb direction

                        PythiaParameters = cms.PSet(
                            pyquenPythiaDefaultBlock,
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
                            kinematics = cms.vstring('CKIN(3)=170',
                                                     'CKIN(4)=9999'
                                                 )
                        )
)


hiSignal.embeddingMode = True

ProductionFilterSequence = cms.Sequence(hiSignal)
