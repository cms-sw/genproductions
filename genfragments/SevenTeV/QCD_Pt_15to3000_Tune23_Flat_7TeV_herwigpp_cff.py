import FWCore.ParameterSet.Config as cms

from Configuration.Generator.HerwigppDefaults_cfi import *

generator = cms.EDFilter("ThePEGGeneratorFilter",
	herwigDefaultsBlock,
	crossSection = cms.untracked.double(2.309540e+10),
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
		'mkdir /Herwig/Weights',
		'cd /Herwig/Weights',
		'create ThePEG::ReweightMinPT reweightMinPT ReweightMinPT.so',

		'cd /Herwig/MatrixElements/',
		'insert SimpleQCD:MatrixElements[0] MEQCD2to2',
		'insert SimpleQCD:Preweights[0] /Herwig/Weights/reweightMinPT',

		'cd /',
		'set /Herwig/Weights/reweightMinPT:Power 4.5',
		'set /Herwig/Weights/reweightMinPT:Scale 15*GeV',
		'set /Herwig/Cuts/JetKtCut:MinKT 15  *GeV',
		'set /Herwig/Cuts/JetKtCut:MaxKT 3000*GeV',
		'set /Herwig/Cuts/QCDCuts:MHatMin 0.0*GeV',
		'set /Herwig/UnderlyingEvent/MPIHandler:IdenticalToUE 0',
	),
)

configurationMetadata = cms.untracked.PSet(
	version = cms.untracked.string('\$Revision: 1.8 $'),
	name = cms.untracked.string('\$Source: /afs/cern.ch/project/cvs/reps/CMSSW/CMSSW/Configuration/GenProduction/python/Attic/QCD_Pt_15to3000_Tune23_Flat_7TeV_herwigpp_cff.py,v $'),
	annotation = cms.untracked.string('Summer2011 sample with HERWIGPP: QCD dijet production, pThat = 15 .. 3000 GeV, weighted, Tune23')
)

