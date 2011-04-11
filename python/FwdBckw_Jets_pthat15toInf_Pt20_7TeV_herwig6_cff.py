import FWCore.ParameterSet.Config as cms

source = cms.Source("EmptySource")
generator = cms.EDFilter("Herwig6GeneratorFilter",
	comEnergy = cms.double(7000.0),
	crossSection = cms.untracked.double(1),
	doMPInteraction = cms.bool(True),
	emulatePythiaStatusCodes = cms.untracked.bool(True),
	filterEfficiency = cms.untracked.double(1.0),
	herwigHepMCVerbosity = cms.untracked.bool(False),
	herwigVerbosity = cms.untracked.int32(0),
	lhapdfSetPath = cms.untracked.string(''),
	maxEventsToPrint = cms.untracked.int32(0),
	printCards = cms.untracked.bool(False),
	useJimmy = cms.bool(True),

	HerwigParameters = cms.PSet(
		herwigQCDjets = cms.vstring(
			'IPROC     = 1500    ! QCD 2->2 processes', 
			'PTMIN     = 15.     ! minimum pt in hadronic jet',
			'PTMAX     = 999999. ! minimum pt in hadronic jet',
			'MODPDF(1) = 10042   ! PDF set according to LHAGLUE', 
			'MODPDF(2) = 10042   ! CTEQ6L1', 
			'JMUEO     = 1       ! multiparton interaction model',
                        'PTJIM     = 4.040   ! 2.8x(sqrt(s)/1.8TeV)^0.27 @ 7 TeV', 
			'JMRAD(73) = 1.8     ! inverse proton radius squared',
			'PRSOF     = 0.0     ! prob. of a soft underlying event',
			'MAXER     = 1000000 ! max error'
		),
		parameterSets = cms.vstring('herwigQCDjets')
	),
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



