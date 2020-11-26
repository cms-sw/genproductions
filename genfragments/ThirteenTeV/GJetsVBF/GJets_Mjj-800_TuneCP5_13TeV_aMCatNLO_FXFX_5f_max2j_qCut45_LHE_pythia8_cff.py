import FWCore.ParameterSet.Config as cms

from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.MCTunes2017.PythiaCP5Settings_cfi import *
from Configuration.Generator.Pythia8aMCatNLOSettings_cfi import *
from Configuration.Generator.PSweightsPythia.PythiaPSweightsSettings_cfi import *

generator = cms.EDFilter("Pythia8HadronizerFilter",
    maxEventsToPrint = cms.untracked.int32(1),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(13000.0),
    PythiaParameters = cms.PSet(
        pythia8CommonSettingsBlock,
        pythia8CP5SettingsBlock,
        pythia8aMCatNLOSettingsBlock,
        pythia8PSweightsSettingsBlock,
        processParameters = cms.vstring(
            'JetMatching:setMad = off', 
            'JetMatching:scheme = 1', 
            'JetMatching:merge = on', 
            'JetMatching:jetAlgorithm = 2', 
            'JetMatching:etaJetMax = 999.', 
            'JetMatching:coneRadius = 1.', 
            'JetMatching:slowJetPower = 1', 
            'JetMatching:qCut = 45.', 
            'JetMatching:doFxFx = on', 
            'JetMatching:qCutME = 30.', 
            'JetMatching:nQmatch = 5', 
            'JetMatching:nJetMax = 2', 
            'TimeShower:mMaxGamma = 4.0'
        ),
        parameterSets = cms.vstring(
            'pythia8CommonSettings', 
            'pythia8CP5Settings', 
            'pythia8aMCatNLOSettings', 
            'pythia8PSweightsSettings',
            'processParameters'
        )
    )
)

from RecoJets.Configuration.GenJetParticles_cff import genParticlesForJetsNoNu
from RecoJets.Configuration.RecoGenJets_cff import ak4GenJetsNoNu

# Filter out PromptFinalState photons
genParticlesNoGamma = cms.EDFilter('CandPtrSelector',
    src = cms.InputTag('genParticles'),
    cut = cms.string('pdgId != 22 || !isPromptFinalState')
)

genParticlesForJetsNoNuNoGamma = genParticlesForJetsNoNu.clone(
    src = cms.InputTag("genParticlesNoGamma")
)

ak4GenJetsNoNuNoGamma = ak4GenJetsNoNu.clone(
    src = cms.InputTag("genParticlesForJetsNoNuNoGamma")
)

vbfGenJetFilterD = cms.EDFilter("VBFGenJetFilter",
    inputTag_GenJetCollection = cms.untracked.InputTag("ak4GenJetsNoNuNoGamma"),
    maxEta = cms.untracked.double(99999.0),
    minEta = cms.untracked.double(-99999.0),
    minInvMass = cms.untracked.double(800.),
    minPt = cms.untracked.double(30.)
)

from GeneratorInterface.Core.generatorSmeared_cfi import generatorSmeared
from PhysicsTools.HepMCCandAlgos.genParticles_cfi import genParticles

ProductionFilterSequence = cms.Sequence(
    generator+
    cms.SequencePlaceholder('randomEngineStateProducer')+
    cms.SequencePlaceholder('VtxSmeared')+
    generatorSmeared+
    genParticles+
    genParticlesNoGamma+
    genParticlesForJetsNoNuNoGamma+
    ak4GenJetsNoNuNoGamma+
    vbfGenJetFilterD+
    cms.SequencePlaceholder("pgen")
)

SimFilterSequence = cms.Sequence(
    vbfGenJetFilterD+
    cms.SequencePlaceholder("psim")
)
