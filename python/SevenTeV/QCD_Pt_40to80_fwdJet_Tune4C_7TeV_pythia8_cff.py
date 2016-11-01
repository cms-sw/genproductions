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
			'PhaseSpace:pTHatMin = 40  ',
			'PhaseSpace:pTHatMax = 80  ',
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

configurationMetadata = cms.untracked.PSet(
	version = cms.untracked.string('\$Revision: 1.1 $'),
	name = cms.untracked.string('\$Source: /local/reps/CMSSW/CMSSW/Configuration/GenProduction/python/SevenTeV/QCD_Pt_40to80_fwdJet_Tune4C_7TeV_pythia8_cff.py,v $'),
	annotation = cms.untracked.string('Sample with PYTHIA8: QCD dijet production, pThat = 40 .. 80 GeV, Tune4C')
)
