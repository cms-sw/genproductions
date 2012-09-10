import FWCore.ParameterSet.Config as cms

from Configuration.Generator.PythiaUED6TSettings_cfi import *

generator = cms.EDFilter("Pythia6GeneratorFilter",
    crossSection = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    comEnergy = cms.double(7000.0),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('MSEL=1               ! QCD hight pT processes', 
            'CKIN(3)=25.          ! minimum pt hat for hard interactions', 
            'CKIN(4)=40.          ! maximum pt hat for hard interactions'),
        # This is a vector of ParameterSet names to be read, in this order
        parameterSets = cms.vstring('pythiaUESettings', 
            'processParameters')
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
	version = cms.untracked.string('\$Revision: 1.3 $'),
	name = cms.untracked.string('\$Source: /local/reps/CMSSW/CMSSW/Configuration/GenProduction/python/SevenTeV/QCD_Pt_25to40_fwdJet_TuneD6T_7TeV_pythia6_cff.py,v $'),
	annotation = cms.untracked.string('LowPU2010 sample with PYTHIA6: QCD dijet production, fwd jet preselection, pThat = 25 .. 40 GeV, TuneD6T')
)
