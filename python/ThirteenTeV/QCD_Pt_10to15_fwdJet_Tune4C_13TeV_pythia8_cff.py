import FWCore.ParameterSet.Config as cms

generator = cms.EDFilter("Pythia8GeneratorFilter",
	comEnergy = cms.double(13000.0),
	crossSection = cms.untracked.double(1.158202e+09),
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
			'PhaseSpace:pTHatMax = 15  ',
			'Tune:pp 5',
			'Tune:ee 3',

		),
		parameterSets = cms.vstring('processParameters')
	)
)


from PhysicsTools.HepMCCandAlgos.genParticles_cfi import genParticles
from RecoJets.Configuration.GenJetParticles_cff import genParticlesForJets
from RecoJets.JetProducers.ak5GenJets_cfi import ak5GenJets
 
fwdJetSelector = cms.EDFilter("CandViewSelector",
    src = cms.InputTag("ak5GenJets"),
    cut = cms.string("pt > 15 & abs( eta ) < 5 & abs( eta ) > 3")
  )             
        
fwdJetFilter = cms.EDFilter("CandViewCountFilter",
     src = cms.InputTag("fwdJetSelector"),
     minNumber = cms.uint32(1),
  )
          
ProductionFilterSequence = cms.Sequence(generator*genParticles*genParticlesForJets*ak5GenJets*fwdJetSelector*fwdJetFilter)
