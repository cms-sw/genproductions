import FWCore.ParameterSet.Config as cms

from Configuration.Generator.HerwigppDefaults_cfi import *
from Configuration.Generator.HerwigppUE_EE_3C_cfi import *


generator = cms.EDFilter("ThePEGGeneratorFilter",
	herwigDefaultsBlock,
	herwigppUESettingsBlock,
	crossSection = cms.untracked.double(1.705848e+04),
	filterEfficiency = cms.untracked.double(1),

	configFiles = cms.vstring(),
	parameterSets = cms.vstring(
		'herwigppUE_EE_3C_8000GeV',
		'productionParameters',
		'basicSetup',
		'setParticlesStableForDetector',
	),
	productionParameters = cms.vstring(
		'cd /Herwig/MatrixElements/',
		'insert SimpleQCD:MatrixElements[0] MEGammaJet',

		'cd /',
		'set /Herwig/Cuts/JetKtCut:MinKT 30  *GeV',
		'set /Herwig/Cuts/JetKtCut:MaxKT 50  *GeV',
		'set /Herwig/Cuts/QCDCuts:MHatMin 0.0*GeV',
		'set /Herwig/Cuts/PhotonKtCut:MinKT 0.0*GeV',
		'set /Herwig/Cuts/PhotonKtCut:MinEta -5',
		'set /Herwig/Cuts/PhotonKtCut:MaxEta 5',
	),
)

configurationMetadata = cms.untracked.PSet(
	version = cms.untracked.string('\$Revision$'),
	name = cms.untracked.string('\$Source$'),
	annotation = cms.untracked.string('Sumer2012 sample with HERWIGPP: Prompt photon production, pThat = 30 .. 50 GeV, TuneEE3C')
)
