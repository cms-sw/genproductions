import FWCore.ParameterSet.Config as cms

from GeneratorInterface.PyquenInterface.pyquenPythiaDefault_cff import *
from Configuration.Generator.PythiaUESettings_cfi import *

generator = cms.EDFilter("PyquenGeneratorFilter",
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
                                                    pythiaZmumujets = cms.vstring('MSEL = 0 ! users defined processes only',
                                                                              'MSUB(15)=1  !qq->Z0/gamma*+g',
	                                                                          'MSUB(30)=1  !qg->Z0/gamma*+q',
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
                                                                              ),
                                                    kinematics = cms.vstring ("CKIN(3)=30"  #min pthat
                                                                              ),
                                                    parameterSets = cms.vstring('pythiaUESettings',
                                                                                'pythiaZmumujets',
                                                                                'kinematics'),
                                                    ),
                        cFlag = cms.int32(1), ## centrality flag
                        bMin = cms.double(0.0), ## min impact param (fm); valid only if cflag_!=0
                        bMax = cms.double(0.0) ## max impact param (fm); valid only if cflag_!=0
                        )

zgenfilter = cms.EDFilter("PythiaFilter",
    MaxRapidity = cms.untracked.double(2.5),
    MinRapidity = cms.untracked.double(-2.5),
    MinPt = cms.untracked.double(30.),
    ParticleID = cms.untracked.int32(23)
)

ProductionFilterSequence = cms.Sequence(generator*zgenfilter)
