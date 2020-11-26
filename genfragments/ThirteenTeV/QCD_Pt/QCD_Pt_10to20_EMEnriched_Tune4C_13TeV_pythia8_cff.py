import FWCore.ParameterSet.Config as cms

generator = cms.EDFilter("Pythia8GeneratorFilter",
	comEnergy = cms.double(13000.0),
	crossSection = cms.untracked.double(8.838e+00),
	filterEfficiency = cms.untracked.double(0.143),
	maxEventsToPrint = cms.untracked.int32(0),
	pythiaHepMCVerbosity = cms.untracked.bool(False),
	pythiaPylistVerbosity = cms.untracked.int32(0),

	PythiaParameters = cms.PSet(
		processParameters = cms.vstring(
			'Main:timesAllowErrors    = 10000',
			'ParticleDecays:limitTau0 = on',
			'ParticleDecays:tauMax = 10',
			'HardQCD:all = on',
			'PhaseSpace:pTHatMin = 10  ',
			'PhaseSpace:pTHatMax = 20  ',
			'Tune:pp 5',
			'Tune:ee 3',

		),
		parameterSets = cms.vstring('processParameters')
	)
)

genParticlesForFilter = cms.EDProducer("GenParticleProducer",
    saveBarCodes = cms.untracked.bool(True),
    src = cms.InputTag("generator"),
    abortOnUnknownPDGCode = cms.untracked.bool(False)
)

bctoefilter = cms.EDFilter("BCToEFilter",
                           filterAlgoPSet = cms.PSet(eTThreshold = cms.double(10),
                                                     genParSource = cms.InputTag("genParticlesForFilter")
                                                     )
                           )

emenrichingfilter = cms.EDFilter("EMEnrichingFilter",
                                 filterAlgoPSet = cms.PSet(isoGenParETMin=cms.double(5.),
                                                           isoGenParConeSize=cms.double(0.1),
                                                           clusterThreshold=cms.double(5.),
                                                           isoConeSize=cms.double(0.2),
                                                           hOverEMax=cms.double(0.5),
                                                           tkIsoMax=cms.double(5.),
                                                           caloIsoMax=cms.double(10.),
                                                           requireTrackMatch=cms.bool(False),
                                                           genParSource = cms.InputTag("genParticlesForFilter")
                                                           )
                                 )
    

# add your filters to this sequence
ProductionFilterSequence = cms.Sequence(generator * (genParticlesForFilter + ~bctoefilter + emenrichingfilter))

