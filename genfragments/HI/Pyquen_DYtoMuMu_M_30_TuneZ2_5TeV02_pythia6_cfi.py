import FWCore.ParameterSet.Config as cms

from Configuration.Generator.PythiaUEZ2Settings_cfi import *

generator = cms.EDFilter("PyquenGeneratorFilter",
	comEnergy = cms.double(5023.0),
	aBeamTarget = cms.double(208.0),
	protonSide = cms.untracked.int32(2),
	qgpInitialTemperature = cms.double(1.0), ## initial temperature of QGP; allowed range [0.2,2.0]GeV;
	qgpProperTimeFormation = cms.double(0.1), ## proper time of QGP formation; allowed range [0.01,10.0]fm/c;
	hadronFreezoutTemperature = cms.double(0.14),
 	doRadiativeEnLoss = cms.bool(True), ## if true, perform partonic radiative en loss
	doCollisionalEnLoss = cms.bool(False),
	qgpNumQuarkFlavor = cms.int32(0),  ## number of active quark flavors in qgp; allowed values: 0,1,2,3
	numQuarkFlavor = cms.int32(0), ## to be removed
	doIsospin = cms.bool(True),
	angularSpectrumSelector = cms.int32(0), ## angular emitted gluon spectrum :
	embeddingMode = cms.bool(False),
	backgroundLabel = cms.InputTag("generator"), ## ineffective in no mixing
	doQuench = cms.bool(False),
	bFixed = cms.double(0.0), ## fixed impact param (fm); valid only if cflag_=0
	cFlag = cms.int32(0), ## centrality flag
	bMin = cms.double(0.0), ## min impact param (fm); valid only if cflag_!=0
	bMax = cms.double(0.0), ## max impact param (fm); valid only if cflag_!=0
 	pythiaPylistVerbosity = cms.untracked.int32(1),
	pythiaHepMCVerbosity = cms.untracked.bool(True),
	maxEventsToPrint = cms.untracked.int32(0),
	PythiaParameters = cms.PSet(
        	pythiaUESettingsBlock,
        	processParameters = cms.vstring('MSEL=0            !User defined processes', 
                                        'MSUB(1)=1         !Incl Z0/gamma* production', 
                                        'MSTP(43)=3        !Both Z0 and gamma*', 
                                        'MDME(174,1)=0     !Z decay into d dbar', 
                                        'MDME(175,1)=0     !Z decay into u ubar', 
                                        'MDME(176,1)=0     !Z decay into s sbar', 
                                        'MDME(177,1)=0     !Z decay into c cbar', 
                                        'MDME(178,1)=0     !Z decay into b bbar', 
                                        'MDME(179,1)=0     !Z decay into t tbar', 
                                        'MDME(182,1)=0     !Z decay into e- e+', 
                                        'MDME(183,1)=0     !Z decay into nu_e nu_ebar', 
                                        'MDME(184,1)=1     !Z decay into mu- mu+', 
                                        'MDME(185,1)=0     !Z decay into nu_mu nu_mubar', 
                                        'MDME(186,1)=0     !Z decay into tau- tau+', 
                                        'MDME(187,1)=0     !Z decay into nu_tau nu_taubar', 
                                        'CKIN(1)=30.       !Minimum sqrt(s_hat) value (=Z mass)'),
        	# This is a vector of ParameterSet names to be read, in this order
        	parameterSets = cms.vstring('pythiaUESettings', 
            			'processParameters')
	)
)


configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /local/reps/CMSSW/CMSSW/Configuration/GenProduction/python/HI/Pyquen_DYtoMuMu_M_30_TuneZ2_5TeV02_pythia6_cfi.py,v $'),
    annotation = cms.untracked.string('PYQUEN DYmumu Mass 30 Tune Z2 at 5.023 TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
