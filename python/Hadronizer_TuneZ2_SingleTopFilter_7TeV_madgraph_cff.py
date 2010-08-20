import FWCore.ParameterSet.Config as cms

from Configuration.Generator.PythiaUEZ2Settings_cfi import *

generator = cms.EDFilter("Pythia6HadronizerFilter",
    pythiaHepMCVerbosity = cms.untracked.bool(True),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    comEnergy = cms.double(7000.0),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('MSEL=0         ! User defined processes', 
                        'PMAS(5,1)=4.8   ! b quark mass',
                        'PMAS(6,1)=172.5 ! t quark mass',
			'MSTJ(1)=1       ! Fragmentation/hadronization on or off',
			'MSTP(61)=1      ! Parton showering on or off'),
        # This is a vector of ParameterSet names to be read, in this order
        parameterSets = cms.vstring('pythiaUESettings', 
            'processParameters')
    ),
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
	pTMax = cms.double(23.44)
)

ProductionFilterSequence = cms.Sequence(generator * singleTopFilter)

configurationMetadata = cms.untracked.PSet(
	version = cms.untracked.string('$Revision: 1.1 $'),
	name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/Hadronizer_TuneD6T_SingleTopFilter_7TeV_madgraph_cff.py,v $'),
	annotation = cms.untracked.string('MadGraph single top t-channel at 7TeV')
)
