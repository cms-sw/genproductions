import FWCore.ParameterSet.Config as cms

source = cms.Source("EmptySource")

from Configuration.Generator.PythiaUEZ2Settings_cfi import *

generator = cms.EDFilter('Pythia6GeneratorFilter',
	comEnergy = cms.double(7000.0),
	crossSection = cms.untracked.double(1),
	filterEfficiency = cms.untracked.double(1),
	maxEventsToPrint = cms.untracked.int32(0),
	pythiaHepMCVerbosity = cms.untracked.bool(False),
	pythiaPylistVerbosity = cms.untracked.int32(0),

	PythiaParameters = cms.PSet(
		pythiaUESettingsBlock,
		processParameters = cms.vstring(
			'MSEL = 1        ! QCD hight pT processes',
			'CKIN(3) = 15    ! minimum pt hat for hard interactions',
			'CKIN(4) = -1    ! maximum pt hat for hard interactions',
		),
		parameterSets = cms.vstring(
			'pythiaUESettings',
			'processParameters',
		)
	)
)

from PhysicsTools.HepMCCandAlgos.genParticles_cfi import genParticles
from RecoJets.Configuration.GenJetParticles_cff import genParticlesForJets
from RecoJets.JetProducers.ak5GenJets_cfi import ak5GenJets

jetfilter1 = cms.EDFilter("EtaPtMinCandViewSelector",
    src = cms.InputTag("ak5GenJets"),                        
    ptMin = cms.double( 20.0 ),                              
    etaMin = cms.double( 2.8 ),                              
    etaMax = cms.double( 10.0 )                              
)                                                            
Filter1 = cms.EDFilter("CandViewCountFilter",        
    src = cms.InputTag("jetfilter1"),                        
    minNumber = cms.uint32(1)                                
)                                                            

jetfilter2 = cms.EDFilter("EtaPtMinCandViewSelector",
    src = cms.InputTag("ak5GenJets"),                        
    ptMin = cms.double( 20.0 ),                              
    etaMin = cms.double( -10.0 ),                            
    etaMax = cms.double( -2.8 )                              
)
Filter2 = cms.EDFilter("CandViewCountFilter",
    src = cms.InputTag("jetfilter2"),
    minNumber = cms.uint32(1)
)

ProductionFilterSequence = cms.Sequence(generator*genParticles*genParticlesForJets*ak5GenJets*jetfilter1*Filter1*jetfilter2*Filter2)


