import FWCore.ParameterSet.Config as cms

from Configuration.Generator.HerwigppDefaults_cfi import *

generator = cms.EDFilter("ThePEGGeneratorFilter",
	herwigDefaultsBlock,
	crossSection = cms.untracked.double(8.832330e-02),
	filterEfficiency = cms.untracked.double(1),

	configFiles = cms.vstring(),
	parameterSets = cms.vstring(
		'cm7TeV',
		'pdfMRST2001',
		'productionParameters',
		'basicSetup',
		'setParticlesStableForDetector',
	),
	productionParameters = cms.vstring(
		'cd /Herwig/MatrixElements/',
		'create Herwig::MEPP2ZJet MEZJet',
		'insert SimpleQCD:MatrixElements[0] MEZJet',
		'set /Herwig/MatrixElements/MEZJet:ZDecay 5',

		'cd /',
		'set /Herwig/Cuts/JetKtCut:MinKT 300 *GeV',
	),
)

configurationMetadata = cms.untracked.PSet(
	version = cms.untracked.string('\$Revision$'),
	name = cms.untracked.string('\$Source$'),
	annotation = cms.untracked.string('Summer2011 sample with HERWIGPP: Z + Jet production, Z -> ee, pThat > 300 GeV, Tune23')
)
