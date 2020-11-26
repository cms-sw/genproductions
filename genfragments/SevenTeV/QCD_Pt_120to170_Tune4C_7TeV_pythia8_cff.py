import FWCore.ParameterSet.Config as cms

generator = cms.EDFilter("Pythia8GeneratorFilter",
	comEnergy = cms.double(7000.0),
	crossSection = cms.untracked.double(1.287613e+05),
	filterEfficiency = cms.untracked.double(1),
	maxEventsToPrint = cms.untracked.int32(0),
	pythiaHepMCVerbosity = cms.untracked.bool(False),
	pythiaPylistVerbosity = cms.untracked.int32(0),

	PythiaParameters = cms.PSet(
		processParameters = cms.vstring(
			'Main:timesAllowErrors    = 10000',
			'ParticleDecays:limitTau0 = on',
			'ParticleDecays:tauMax = 10',
			'HardQCD:all = on',
			'PhaseSpace:pTHatMin = 120 ',
			'PhaseSpace:pTHatMax = 170 ',
			'Tune:pp 5',
			'Tune:ee 3',

		),
		parameterSets = cms.vstring('processParameters')
	)
)

configurationMetadata = cms.untracked.PSet(
	version = cms.untracked.string('\$Revision: 1.1 $'),
	name = cms.untracked.string('\$Source: /local/reps/CMSSW/CMSSW/Configuration/GenProduction/python/SevenTeV/QCD_Pt_120to170_Tune4C_7TeV_pythia8_cff.py,v $'),
	annotation = cms.untracked.string('Sample with PYTHIA8: QCD dijet production, pThat = 120 .. 170 GeV, Tune4C')
)
