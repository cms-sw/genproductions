import FWCore.ParameterSet.Config as cms

from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.Pythia8CUEP8M1Settings_cfi import *

generator = cms.EDFilter("Pythia8GeneratorFilter",
                         comEnergy = cms.double(13000.0),
                         crossSection = cms.untracked.double(2.237e+09),
                         filterEfficiency = cms.untracked.double(0.246),
                         maxEventsToPrint = cms.untracked.int32(0),
                         pythiaHepMCVerbosity = cms.untracked.bool(False),
                         pythiaPylistVerbosity = cms.untracked.int32(0),
                         PythiaParameters = cms.PSet(
        pythia8CUEP8M1SettingsBlock,
        pythia8CommonSettingsBlock,
        processParameters = cms.vstring(
            'HardQCD:all = on',
            'PhaseSpace:pTHatMin = 70  ',
            'PhaseSpace:pTHatMax = 6500  ',
            'Tune:pp 14',
            'Tune:ee 7',

            ),
        parameterSets = cms.vstring('pythia8CommonSettings','pythia8CUEP8M1Settings','processParameters')
        )
)


from PhysicsTools.HepMCCandAlgos.genParticles_cfi import genParticles
from RecoJets.Configuration.GenJetParticles_cff import genParticlesForJets
from RecoJets.JetProducers.ak4GenJets_cfi import ak4GenJets
 
fwdJetSelector = cms.EDFilter("CandViewSelector",
    src = cms.InputTag("ak4GenJets"),
    cut = cms.string("pt > 70 & abs( eta ) < 5.5 & abs( eta ) > 2.5")
  )             
        
fwdJetFilter = cms.EDFilter("CandViewCountFilter",
     src = cms.InputTag("fwdJetSelector"),
     minNumber = cms.uint32(1),
  )
          
ProductionFilterSequence = cms.Sequence(generator*genParticles*genParticlesForJets*ak4GenJets*fwdJetSelector*fwdJetFilter)
