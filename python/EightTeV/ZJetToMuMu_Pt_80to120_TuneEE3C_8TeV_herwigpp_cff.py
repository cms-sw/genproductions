import FWCore.ParameterSet.Config as cms

from Configuration.Generator.HerwigppDefaults_cfi import *
from Configuration.Generator.HerwigppUE_EE_3C_cfi import *


generator = cms.EDFilter("ThePEGGeneratorFilter",
	herwigDefaultsBlock,
	herwigppUESettingsBlock,
	crossSection = cms.untracked.double(9.745931e+00),
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
		'create Herwig::MEPP2ZJet MEZJet',
		'insert SimpleQCD:MatrixElements[0] MEZJet',
		'set /Herwig/MatrixElements/MEZJet:ZDecay 6',

		'cd /',
		'set /Herwig/Cuts/QCDCuts:MHatMin 0.0*GeV',
		'set /Herwig/Cuts/JetKtCut:MinKT 80  *GeV',
		'set /Herwig/Cuts/JetKtCut:MaxKT 120 *GeV',
	),
)

configurationMetadata = cms.untracked.PSet(
	version = cms.untracked.string('\$Revision$'),
	name = cms.untracked.string('\$Source$'),
	annotation = cms.untracked.string('Sumer2012 sample with HERWIGPP: Z + Jet production, Z -> mumu, pThat = 80 .. 120 GeV, TuneEE3C')
)
