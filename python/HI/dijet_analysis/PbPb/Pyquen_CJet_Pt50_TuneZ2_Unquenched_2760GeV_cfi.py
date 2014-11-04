import FWCore.ParameterSet.Config as cms


from Configuration.Generator.PyquenTuneZ2Settings_cff import *

cjetTrigCommon = cms.PSet(filterType = cms.untracked.string("EcalGenEvtSelector"),
                          etaMax     = cms.double(3),
                          partons    = cms.vint32(4),
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
                                                      2, 2
                                                      ),
                          particles = cms.vint32(411, -411, # D+
                                                 413, -413, # D*+
                                                 421, -421, # D0
                                                 423, -423, # D*0
                                                 431, -431, # Ds+
                                                 433, -433, # Ds*+
                                                 441, -441, # etac
                                                 443, -443, # J/Psi
                                                 451, -451, # etac
                                                 4112, -4112, # Sigma_c-
                                                 4114, -4114, # Sigma*_c-
                                                 4122, -4122, # Lambda_c0
                                                 4212, -4212, # Sigma_c0
                                                 4214, -4214, # Sigma*_c0
                                                 4222, -4222, # Sigma_c+
                                                 4224, -4224 # Sigma*_c+
                                                 ),
                          particlePt = cms.vdouble(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
                                                   )
                          )


hiSignal = cms.EDFilter("PyquenGeneratorFilter",
                        cjetTrigCommon,
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
                                                                              'CKIN(3) = 50.       !(D=0 GeV) lower lim pT_hat',
                                                                              'CKIN(4) = 9999.       !(D=-1 GeV) upper lim pT_hat, if < 0 innactive',
                                                                              ),
                                                    pythiaDijet = cms.vstring ("MSEL=1" # dijets
                                                                               ),
                                                    
                                                    )
                       
                        )

hiSignal.embeddingMode = True

ProductionFilterSequence = cms.Sequence(hiSignal)
