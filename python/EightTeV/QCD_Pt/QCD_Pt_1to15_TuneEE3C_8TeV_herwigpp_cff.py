import FWCore.ParameterSet.Config as cms

from Configuration.Generator.HerwigppDefaults_cfi import *
from Configuration.Generator.HerwigppUE_EE_3C_cfi import *

generator = cms.EDFilter("ThePEGGeneratorFilter",
	herwigDefaultsBlock,
	herwigppUESettingsBlock,
	crossSection = cms.untracked.double(8.478808e+08),
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
		'insert SimpleQCD:MatrixElements[0] MEQCD2to2',
		'cd /',
		'set /Herwig/Cuts/JetKtCut:MinKT 1  *GeV',
		'set /Herwig/Cuts/JetKtCut:MaxKT 15*GeV',
		'set /Herwig/Cuts/QCDCuts:MHatMin 0.0*GeV',
		'set /Herwig/UnderlyingEvent/MPIHandler:IdenticalToUE 0',
	),
)

configurationMetadata = cms.untracked.PSet(
	version = cms.untracked.string('\$Revision: 1.1 $'),
	name = cms.untracked.string('\$Source: /local/reps/CMSSW/CMSSW/Configuration/GenProduction/python/EightTeV/QCD_Pt_1to15_TuneEE3C_8TeV_herwigpp_cff.py,v $'),
	annotation = cms.untracked.string('Summer2012 sample with HERWIGPP: dijet production, pThat = 1 .. 15 GeV, TuneEE3C')
)
