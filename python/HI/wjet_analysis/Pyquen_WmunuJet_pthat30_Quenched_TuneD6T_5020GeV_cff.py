import FWCore.ParameterSet.Config as cms

from GeneratorInterface.PyquenInterface.pyquenPythiaDefault_cff import *
from Configuration.Generator.PythiaUESettings_cfi import *


process.generator = cms.EDFilter("PyquenGeneratorFilter",
                        aBeamTarget = cms.double(208.0), ## beam/target atomic number
                        comEnergy = cms.double(5020.0),
                        
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
                        
                        embeddingMode = cms.bool(False),
			backgroundLabel = cms.InputTag("generator"),
                        
                        bFixed = cms.double(0.0), ## fixed impact param (fm); valid only if cflag_=0
                        PythiaParameters = cms.PSet(pythiaUESettingsBlock,
                                                    pythiaWenujets = cms.vstring('MSEL = 0 ! users defined processes only',
										 'MSUB(16)     = 1    !qW production', 
										 'MSUB(31)     = 1    !gW production', 
										 'MDME(190,1) = 0    !W decay into dbar u', 
										 'MDME(191,1) = 0    !W decay into dbar c', 
										 'MDME(192,1) = 0    !W decay into dbar t', 
										 'MDME(194,1) = 0    !W decay into sbar u', 
										 'MDME(195,1) = 0    !W decay into sbar c', 
										 'MDME(196,1) = 0    !W decay into sbar t', 
										 'MDME(198,1) = 0    !W decay into bbar u', 
										 'MDME(199,1) = 0    !W decay into bbar c', 
										 'MDME(200,1) = 0    !W decay into bbar t', 
										 'MDME(206,1) = 0    !W decay into e+ nu_e', 
										 'MDME(207,1) = 1    !W decay into mu+ nu_mu', 
										 'MDME(208,1) = 0    !W decay into tau+ nu_tau'
                                                                              ),
                                                    kinematics = cms.vstring ("CKIN(3)=30"  #min pthat
                                                                              ),
                                                    parameterSets = cms.vstring('pythiaUESettings',
                                                                                'pythiaWenujets',
                                                                                'kinematics'),
                                                    ),
                        cFlag = cms.int32(0), ## centrality flag
                        bMin = cms.double(0.0), ## min impact param (fm); valid only if cflag_!=0
                        bMax = cms.double(0.0) ## max impact param (fm); valid only if cflag_!=0
                        )

process.wgenfilter = cms.EDFilter("PythiaFilter",
    MaxRapidity = cms.untracked.double(2.5),
    MinRapidity = cms.untracked.double(-2.5),
    MinPt = cms.untracked.double(30.),
    ParticleID = cms.untracked.int32(24)
)

process.ProductionFilterSequence = cms.Sequence(process.generator*process.wgenfilter)
