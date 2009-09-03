import FWCore.ParameterSet.Config as cms

configurationMetadata = cms.untracked.PSet(
	version = cms.untracked.string('$Revision: 1.1 $'),
	name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/HERWIGPP_MBMPI_10TeV_cff.py,v $'),
	annotation = cms.untracked.string('Summer09: Herwig++ generation of MB events using MPIs, 10TeV, MRST2001')
)

from Configuration.GenProduction.HerwigppDefaults_cfi import *

source = cms.Source("EmptySource")
generator = cms.EDFilter("ThePEGGeneratorFilter",
	herwigDefaultsBlock,

	configFiles = cms.vstring(),
	parameterSets = cms.vstring(
		'cm7TeV',
		'pdfMRST2001',
		'Summer09QCDParameters',
		'basicSetup',
		'setParticlesStableForDetector',
	),

	Summer09QCDParameters = cms.vstring(
		'cd /Herwig/MatrixElements/',
		'insert SimpleQCD:MatrixElements[0] MEMinBias',

		'cd /',
		'cd /Herwig/Cuts',
		'set JetKtCut:MinKT 0.0*GeV',
		'set QCDCuts:MHatMin 0.0*GeV',
		'set QCDCuts:X1Min 0.01',
		'set QCDCuts:X2Min 0.01',
		'set /Herwig/UnderlyingEvent/MPIHandler:IdenticalToUE 0',
	),

	crossSection = cms.untracked.double(119.0e+03),
	filterEfficiency = cms.untracked.double(1.0),
)

ProductionFilterSequence = cms.Sequence(generator)
