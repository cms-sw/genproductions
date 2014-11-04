import FWCore.ParameterSet.Config as cms

from Configuration.Generator.PyquenTuneZ2Settings_cff import *

bjetTrigCommon = cms.PSet(filterType = cms.untracked.string("EcalGenEvtSelector"),
                          etaMax     = cms.double(3),
                          partons    = cms.vint32(5),
                          partonPt   = cms.vdouble(10),
                          partonStatus = cms.vint32(2),
                          particleStatus = cms.vint32(2, 2, 
                                                      2, 2, 
                                                      2, 2,
                                                      2, 2,
                                                      2, 2,                                                      
                                                      2, 2,
                                                      2, 2,
                                                      2, 2,
                                                      2, 2,
                                                      2, 2,                                                      
                                                      2, 2,
                                                      2, 2,
                                                      2, 2,
                                                      2, 2,
                                                      2, 2,
                                                      2, 2,
                                                      2, 2
                                                      ),
                          particles = cms.vint32(511, -511, # B0
                                                 513, -513, # B*0
                                                 521, -521, # B+
                                                 523, -523, # B*+
                                                 531, -531, # Bs0
                                                 533, -533, # Bs*0
                                                 541, -541, # Bc0
                                                 543, -543, # Bc*0
                                                 551, -551, # etab
                                                 553, -553, # Upsilon
                                                 5112, -5112, # Sigma_b-
                                                 5114, -5114, # Sigma*_b-
                                                 5122, -5122, # Lambda_b0
                                                 5212, -5212, # Sigma_b0
                                                 5214, -5214, # Sigma*_b0
                                                 5222, -5222, # Sigma_b+
                                                 5224, -5224 # Sigma*_b+
                                                 ),
                          particlePt = cms.vdouble(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
                                                   )
                          )

hiSignal = cms.EDFilter("PyquenGeneratorFilter",
                        bjetTrigCommon,
                        collisionParameters,
                        qgpParameters,
                        pyquenParameters,
                        doQuench = cms.bool(False),
                        bFixed = cms.double(0.0), ## fixed impact param (fm); valid only if cflag_=0
                        cFlag = cms.int32(0), ## centrality flag
                        bMin = cms.double(0.0), ## min impact param (fm); valid only if cflag_!=0
                        bMax = cms.double(0.0), ## max impact param (fm); valid only if cflag_!=0
                        PythiaParameters = cms.PSet(pyquenPythiaDefaultBlock,
                                                    parameterSets = cms.vstring('pythiaUESettings',
                                                                                'pythiaDijet',
                                                                                'kinematics'
                                                                                ),
                                                    kinematics = cms.vstring ("CKIN(7)=-3.",  #min rapidity
                                                                              "CKIN(8)=3.",    #max rapidity
                                                                              'CKIN(3) = 80.       !(D=0 GeV) lower lim pT_hat',
                                                                              'CKIN(4) = 9999.       !(D=-1 GeV) upper lim pT_hat, if < 0 innactive',
                                                                              ),
                                                    pythiaDijet = cms.vstring ("MSEL=1" # dijets
                                                                               ),
                                                    
                                                    )
                       
                        )

hiSignal.embeddingMode = True

ProductionFilterSequence = cms.Sequence(hiSignal)
