import FWCore.ParameterSet.Config as cms

from Configuration.Generator.HerwigppDefaults_cfi import *
from Configuration.Generator.HerwigppUE_EE_3C_cfi import *


generator = cms.EDFilter("ThePEGGeneratorFilter",
	herwigDefaultsBlock,
	herwigppUESettingsBlock,
	crossSection = cms.untracked.double(8.478808e+08),
	filterEfficiency = cms.untracked.double(1),

	configFiles = cms.vstring(),
	parameterSets = cms.vstring(
		'herwigppUE_EE_3C_8000GeV',
		'productionParameters',
		'basicSetup',
		'setParticlesStableForDetector',
	),
	productionParameters = cms.vstring(

		'cd /Herwig/MatrixElements/',
		'insert SimpleQCD:MatrixElements[0] MEQCD2to2',
		'cd /',
		'set /Herwig/Cuts/JetKtCut:MinKT 15  *GeV',
		'set /Herwig/Cuts/JetKtCut:MaxKT 30000000*GeV',
		'set /Herwig/Cuts/QCDCuts:MHatMin 0.0*GeV',
		'set /Herwig/UnderlyingEvent/MPIHandler:IdenticalToUE 0',
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


configurationMetadata = cms.untracked.PSet(
	version = cms.untracked.string('\$Revision: 1.1 $'),
	name = cms.untracked.string('\$Source: /cvs/CMSSW/CMSSW/Configuration/GenProduction/python/EightTeV/QCD_FwdBckwJets_Pt_15toInf_TuneEE3C_Flat_8TeV_herwigpp_cff.py,v $'),
	annotation = cms.untracked.string('Sumer2012 sample with HERWIGPP: QCD Fwd-Backw dijet production, pThat = 15 .. Inf GeV, TuneEE3C')
)
