import FWCore.ParameterSet.Config as cms
from Configuration.GenProduction.PythiaUESettings_cfi import *

source = cms.Source("PoolSource",
	fileNames = cms.untracked.vstring('file:LHEtoEDM.root'),
	skipEvents = cms.untracked.uint32(0)
)

generator = cms.EDProducer("LHEProducer",
	hadronisation = cms.PSet(
		pythiaUESettingsBlock,
		generator = cms.string('Pythia6'),
		parameterSets = cms.vstring(
			'pythiaUESettings',
			'pythiaCMSDefaults'
		),
		pythiaCMSDefaults = cms.vstring(
			'PMAS(5,1)=4.4    ! b quarks mass', 
			'PMAS(6,1)=172.4  ! t quarks mass', 
			'MSTJ(1)=1        !...Fragmentation/hadronization on or off', 
			'MSTP(61)=1       ! Parton showering on or off', 
			'MSEL=0           ! User defined processes/Full user control'
		)
	)
)

singleTopFilter = cms.EDFilter("STFilter",
	produceHistos = cms.bool(False),
	TH1bEtaFiltered = cms.PSet(
		xmin = cms.double(-8.0),
		Nbinx = cms.int32(100),
		xmax = cms.double(8.0)
	),
	histOutFile = cms.untracked.string(''),
	TH1bPt = cms.PSet(
		xmin = cms.double(0.0),
		Nbinx = cms.int32(150),
		xmax = cms.double(150.0)
	),
	debuglvl = cms.untracked.int32(0), ## debug level (0=none, 1=small, 2=extended, 3 = very extended)

	TH1bEta = cms.PSet(
		xmin = cms.double(-8.0),
		Nbinx = cms.int32(100),
		xmax = cms.double(8.0)
	),
	TH1bPtFiltered = cms.PSet(
		xmin = cms.double(0.0),
		Nbinx = cms.int32(150),
		xmax = cms.double(150.0)
	),
	pTMax = cms.double(15.84)
)

ProducerSourceSequence = cms.Sequence(generator * singleTopFilter)

configurationMetadata = cms.untracked.PSet(
	version = cms.untracked.string('$Revision: 1.5 $'),
	name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/MadGraph_XQCUT15_10TeV_GEN_cff.py,v $'),
	annotation = cms.untracked.string('MadGraph single top t-channel at 10TeV')
)
