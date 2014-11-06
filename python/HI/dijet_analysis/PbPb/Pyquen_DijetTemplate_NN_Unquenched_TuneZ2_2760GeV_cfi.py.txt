import FWCore.ParameterSet.Config as cms

hiSignal = cms.EDFilter("PyquenGeneratorFilter",
                        aBeamTarget = cms.double(208.0), ## beam/target atomic number
                        comEnergy = cms.double(2760.0),
                        
                        qgpInitialTemperature = cms.double(1.0), ## initial temperature of QGP; allowed range [0.2,2.0]GeV;
                        qgpProperTimeFormation = cms.double(0.1), ## proper time of QGP formation; allowed range [0.01,10.0]fm/c;
                        hadronFreezoutTemperature = cms.double(0.14),
                        doRadiativeEnLoss = cms.bool(True), ## if true, perform partonic radiative en loss
                        doCollisionalEnLoss = cms.bool(False),
                        qgpNumQuarkFlavor = cms.int32(0),  ## number of active quark flavors in qgp; allowed values: 0,1,2,3
                        numQuarkFlavor = cms.int32(0), ## to be removed
                        
                        angularSpectrumSelector = cms.int32(0), ## angular emitted gluon spectrum :
                        embeddingMode = cms.bool(True),
                        backgroundLabel = cms.InputTag("generator"), ## ineffective in no mixing
                        
                        doQuench = cms.bool(False),
                        doIsospin = cms.bool(True),
                        
                        bFixed = cms.double(0.0), ## fixed impact param (fm); valid only if cflag_=0
                        PythiaParameters = cms.PSet(parameterSets = cms.vstring('pythiaUESettings',
                                                                                'ppJets',
                                                                                'kinematics'),
                                                    pythiaUESettings = cms.vstring('MSTU(21)=1     ! Check on possible errors during program execution',
                                                                                   'MSTJ(22)=2     ! Decay those unstable particles',
                                                                                   'PARJ(71)=10 .  ! for which ctau  10 mm',
                                                                                   'MSTP(33)=0     ! no K factors in hard cross sections',
                                                                                   'MSTP(2)=1      ! which order running alphaS',
                                                                                   'MSTP(51)=10042 ! structure function chosen (external PDF CTEQ6L1)',
                                                                                   'MSTP(52)=2     ! work with LHAPDF',
                                                                                   
                                                                                   'PARP(82)=1.832 ! pt cutoff for multiparton interactions',
                                                                                   'PARP(89)=1800. ! sqrts for which PARP82 is set',
                                                                                   'PARP(90)=0.275 ! Multiple interactions: rescaling power',
                                                                                   
                                                                                   'MSTP(95)=6     ! CR (color reconnection parameters)',
                                                                                   'PARP(77)=1.016 ! CR',
                                                                                   'PARP(78)=0.538 ! CR',
                                                                                   
                                                                                   'PARP(80)=0.1   ! Prob. colored parton from BBR',
                                                                                   
                                                                                   'PARP(83)=0.356 ! Multiple interactions: matter distribution parameter',
                                                                                   'PARP(84)=0.651 ! Multiple interactions: matter distribution parameter',
                                                                                   
                                                                                   'PARP(62)=1.025 ! ISR cutoff',
                                                                                   
                                                                                   'MSTP(91)=1     ! Gaussian primordial kT',
                                                                                   'PARP(93)=10.0  ! primordial kT-max',
                                                                                   
                                                                                   'MSTP(81)=21    ! multiple parton interactions 1 is Pythia default',
                                                                                   'MSTP(82)=4     ! Defines the multi-parton model',
                                                                                   ),
                                                    ppJets = cms.vstring('MSEL=1   ! QCD hight pT processes'),
                                                    kinematics = cms.vstring ("CKIN(3)=123456789",  #min pthat
                                                                              "CKIN(4)=9999" #max pthat
                                                                              )
                                                    ),
                        cFlag = cms.int32(0), ## centrality flag
                        bMin = cms.double(0.0), ## min impact param (fm); valid only if cflag_!=0
                        bMax = cms.double(0.0) ## max impact param (fm); valid only if cflag_!=0
                        )

configurationMetadata = cms.untracked.PSet(
    annotation = cms.untracked.string('PYTHIA (unquenched) dijets in NN (pt-hat > 123456789 GeV) at sqrt(s) = 2.76TeV')
    )

ProductionFilterSequence = cms.Sequence(hiSignal)
