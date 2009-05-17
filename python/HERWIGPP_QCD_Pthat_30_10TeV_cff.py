import FWCore.ParameterSet.Config as cms

configurationMetadata = cms.untracked.PSet(
	version = cms.untracked.string(': 1.1 $'),
	name = cms.untracked.string(': /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/Herwigpp_QCD_kt.template,v $'),
	annotation = cms.untracked.string('Summer09: Herwig++ generation of QCD events, 10TeV, MRST2001, pthat > 30 GeV')
)

from Configuration.GenProduction.HerwigppDefaults_cfi import *

source = cms.Source("EmptySource")
generator = cms.EDFilter("ThePEGGeneratorFilter",
	herwigDefaultsBlock,

	configFiles = cms.vstring(),
	parameterSets = cms.vstring(
		'cm10TeV',
		'pdfMRST2001',
		'Summer09QCDParameters',
		'basicSetup',
		'setParticlesStableForDetector',
	),

	Summer09QCDParameters = cms.vstring(
		'cd /Herwig/MatrixElements/',
		'insert SimpleQCD:MatrixElements[0] MEQCD2to2',

		'cd /',
		'set /Herwig/Cuts/JetKtCut:MinKT 30*GeV',
		'set /Herwig/UnderlyingEvent/MPIHandler:Algorithm 1',
	),

	crossSection = cms.untracked.double(1.1174600e+08),
	filterEfficiency = cms.untracked.double(1.0),
)

ProductionFilterSequence = cms.Sequence(generator)
