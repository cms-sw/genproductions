import FWCore.ParameterSet.Config as cms

generator = cms.EDFilter("Pythia8GeneratorFilter",
	comEnergy = cms.double(7000.0),
	crossSection = cms.untracked.double(9.573213e+08),
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
			'PhaseSpace:pTHatMin = 10  ',
			'PhaseSpace:pTHatMax = 25  ',
			'Tune:pp 5',
			'Tune:ee 3',

		),
		parameterSets = cms.vstring('processParameters')
	)
)

from PhysicsTools.HepMCCandAlgos.genParticles_cfi import genParticles
from RecoJets.Configuration.GenJetParticles_cff import genParticlesForJets
from RecoJets.JetProducers.ak5GenJets_cfi import ak5GenJets
                
jetfilter = cms.EDFilter("EtaPtMinCandViewSelector",
    src = cms.InputTag("ak5GenJets"), 
    ptMin = cms.double( 15.0 ),
    etaMin = cms.double( 3.0),
    etaMax = cms.double( 5.0 )
)
                  
Filter = cms.EDFilter("CandViewCountFilter",
    src = cms.InputTag("jetfilter"),
    minNumber = cms.uint32(1)
)
         
ProductionFilterSequence = cms.Sequence(generator*genParticles*genParticlesForJets*ak5GenJets*jetfilter*Filter)

configurationMetadata = cms.untracked.PSet(
	version = cms.untracked.string('\$Revision: 1.1 $'),
	name = cms.untracked.string('\$Source: /local/reps/CMSSW/CMSSW/Configuration/GenProduction/python/SevenTeV/QCD_Pt_10to25_fwd_JetTune4C_7TeV_pythia8_cff.py,v $'),
	annotation = cms.untracked.string('Sample with PYTHIA8: QCD dijet production, pThat = 10 .. 25 GeV, Tune4C')
)
