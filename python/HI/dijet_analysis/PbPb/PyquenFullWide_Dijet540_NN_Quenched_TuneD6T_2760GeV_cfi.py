import FWCore.ParameterSet.Config as cms

hiSignal = cms.EDFilter("PyquenGeneratorFilter",
                        aBeamTarget = cms.double(208.0), ## beam/target atomic number
                        comEnergy = cms.double(2760.0),
                        
                        qgpInitialTemperature = cms.double(1.0), ## initial temperature of QGP; allowed range [0.2,2.0]GeV;
                        qgpProperTimeFormation = cms.double(0.1), ## proper time of QGP formation; allowed range [0.01,10.0]fm/c;
                        hadronFreezoutTemperature = cms.double(0.14),
                        qgpNumQuarkFlavor = cms.int32(0),  ## number of active quark flavors in qgp; allowed values: 0,1,2,3
                        numQuarkFlavor = cms.int32(0), ## to be removed                        
                        
                        doQuench = cms.bool(True),
                        doRadiativeEnLoss = cms.bool(True), ## if true, perform partonic radiative en loss
                        doCollisionalEnLoss = cms.bool(True),
                        angularSpectrumSelector = cms.int32(1), ## angular emitted gluon spectrum :
                        
                        doIsospin = cms.bool(True),
                        embeddingMode = cms.bool(True),
			backgroundLabel = cms.InputTag("generator"),
                        
                        bFixed = cms.double(0.0), ## fixed impact param (fm); valid only if cflag_=0
                        PythiaParameters = cms.PSet(parameterSets = cms.vstring('pythiaUESettings',
                                                                                'ppJets',
                                                                                'kinematics'),

                                                    pythiaUESettings = cms.vstring('MSTJ(11)=3     ! Choice of the fragmentation function',
                                                                                   'MSTJ(22)=2     ! Decay those unstable particles',
                                                                                   'PARJ(71)=10 .  ! for which ctau  10 mm',
                                                                                   'MSTP(2)=1      ! which order running alphaS',
                                                                                   'MSTP(33)=0     ! no K factors in hard cross sections',
                                                                                   'MSTP(51)=10042 ! structure function chosen (external PDF CTEQ6L1)',
                                                                                   'MSTP(52)=2     ! work with LHAPDF',
                                                                                   'MSTP(81)=1     ! multiple parton interactions 1 is Pythia default',
                                                                                   'MSTP(82)=4     ! Defines the multi-parton model',
                                                                                   'MSTU(21)=1     ! Check on possible errors during program execution',
                                                                                   'PARP(82)=1.8387   ! pt cutoff for multiparton interactions',
                                                                                   'PARP(89)=1960. ! sqrts for which PARP82 is set',
                                                                                   'PARP(83)=0.5   ! Multiple interactions: matter distrbn parameter',
                                                                                   'PARP(84)=0.4   ! Multiple interactions: matter distribution parameter',
                                                                                   'PARP(90)=0.16  ! Multiple interactions: rescaling power',
                                                                                   'PARP(67)=2.5    ! amount of initial-state radiation',
                                                                                   'PARP(85)=1.0  ! gluon prod. mechanism in MI',
                                                                                   'PARP(86)=1.0  ! gluon prod. mechanism in MI',
                                                                                   'PARP(62)=1.25   ! ',
                                                                                   'PARP(64)=0.2    ! ',
                                                                                   'MSTP(91)=1      !',
                                                                                   'PARP(91)=2.1   ! kt distribution',
                                                                                   'PARP(93)=15.0  ! '
                                                                                   ),
                                                    ppJets = cms.vstring('MSEL=1   ! QCD hight pT processes'),
                                                    kinematics = cms.vstring ("CKIN(3)=540",  #min pthat
                                                                              "CKIN(4)=9999" #max pthat
                                                                              )
                                                    ),
                        cFlag = cms.int32(0), ## centrality flag
                        bMin = cms.double(0.0), ## min impact param (fm); valid only if cflag_!=0
                        bMax = cms.double(0.0) ## max impact param (fm); valid only if cflag_!=0
                        )

configurationMetadata = cms.untracked.PSet(
    annotation = cms.untracked.string('PYTHIA (unquenched) dijets in NN (pt-hat > 540 GeV) at sqrt(s) = 2.76TeV')
    )

ProductionFilterSequence = cms.Sequence(hiSignal)
