import FWCore.ParameterSet.Config as cms

generator = cms.EDFilter("Pythia8GeneratorFilter",
	comEnergy = cms.double(8000.0),
	crossSection = cms.untracked.double(1.938868e+03),
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
			'PhaseSpace:pTHatMin = 300 ',
			'PhaseSpace:pTHatMax = 470 ',
			'Tune:pp 5',
			'Tune:ee 3',

		),
		parameterSets = cms.vstring('processParameters')
	)
)

configurationMetadata = cms.untracked.PSet(
	version = cms.untracked.string('\$Revision$'),
	name = cms.untracked.string('\$Source$'),
	annotation = cms.untracked.string('Summer2012 sample with PYTHIA8: QCD dijet production, pThat = 300 .. 470 GeV, Tune4C')
)
