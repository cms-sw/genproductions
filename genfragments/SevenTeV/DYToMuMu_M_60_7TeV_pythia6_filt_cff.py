import FWCore.ParameterSet.Config as cms

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs/CMSSW/CMSSW/Configuration/GenProduction/python/SevenTeV/DYToMuMu_M_60_7TeV_pythia6_filt_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 Z/gamma* to mumu, M(mu+mu-) > 60 GeV at sqrt(s) = 7TeV')
)


generator = cms.EDFilter("Pythia6GeneratorFilter",
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    PythiaParameters = cms.PSet(
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
                                           'PARP(93)=15.0  ! '),
            processParameters = cms.vstring('MSEL         = 11 ',
                                            'MDME( 174,1) = 0    !Z decay into d dbar',
                                            'MDME( 175,1) = 0    !Z decay into u ubar',
                                            'MDME( 176,1) = 0    !Z decay into s sbar',
                                            'MDME( 177,1) = 0    !Z decay into c cbar',
                                            'MDME( 178,1) = 0    !Z decay into b bbar',
                                            'MDME( 179,1) = 0    !Z decay into t tbar',
                                            'MDME( 182,1) = 0    !Z decay into e- e+',
                                            'MDME( 183,1) = 0    !Z decay into nu_e nu_ebar',
                                            'MDME( 184,1) = 1    !Z decay into mu- mu+',
                                            'MDME( 185,1) = 0    !Z decay into nu_mu nu_mubar',
                                            'MDME( 186,1) = 0    !Z decay into tau- tau+',
                                            'MDME( 187,1) = 0    !Z decay into nu_tau nu_taubar',
                                            'CKIN( 1)     = 60.  !(D=2. GeV)',
                                            'CKIN( 2)     = -1.  !(D=-1. GeV)'),
                    parameterSets = cms.vstring('pythiaUESettings',
                                                            'processParameters')
                )
)

ZpTFilter = cms.EDFilter("PythiaFilter",
                         ParticleID = cms.untracked.int32(23),
                         MinPt = cms.untracked.double(5.0)
)

zmumugenfilter = cms.EDFilter("MCZll",
                              leptonFlavour = cms.untracked.int32(13),
                              leptonPtMin = cms.untracked.double(10.0),
                              leptonEtaMax = cms.untracked.double(2.1),
                              zMassMax = cms.untracked.double(99999.)
)

ProductionFilterSequence = cms.Sequence(generator*ZpTFilter*zmumugenfilter)
