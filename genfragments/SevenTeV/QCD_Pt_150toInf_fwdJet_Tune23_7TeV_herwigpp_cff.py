import FWCore.ParameterSet.Config as cms

from Configuration.Generator.HerwigppDefaults_cfi import *

generator = cms.EDFilter("ThePEGGeneratorFilter",
	herwigDefaultsBlock,
	crossSection = cms.untracked.double(1),
	filterEfficiency = cms.untracked.double(1),

	configFiles = cms.vstring(),
	parameterSets = cms.vstring(
		'cm7TeV',
		'pdfMRST2001',
		'productionParameters',
		'basicSetup',
		'setParticlesStableForDetector',
	),
	productionParameters = cms.vstring(
		'cd /Herwig/MatrixElements/',
		'insert SimpleQCD:MatrixElements[0] MEQCD2to2',

		'cd /',
		'set /Herwig/Cuts/JetKtCut:MinKT 150  *GeV',
		'set /Herwig/Cuts/JetKtCut:MaxKT 15000000  *GeV',
		'set /Herwig/Cuts/QCDCuts:MHatMin 0.0*GeV',
		'set /Herwig/UnderlyingEvent/MPIHandler:IdenticalToUE 0',
	),
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
	version = cms.untracked.string('\$Revision: 1.8 $'),
	name = cms.untracked.string('\$Source: /afs/cern.ch/project/cvs/reps/CMSSW/CMSSW/Configuration/GenProduction/python/Attic/QCD_Pt_150toInf_fwdJet_Tune23_7TeV_herwigpp_cff.py,v $'),
	annotation = cms.untracked.string('LowPU2010 sample with HERWIGPP: QCD dijet production, pThat = 150 .. Inf GeV, Tune23')
)

