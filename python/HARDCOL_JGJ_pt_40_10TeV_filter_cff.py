import FWCore.ParameterSet.Config as cms

source = cms.Source("MCDBSource",
        articleID = cms.uint32(327),
        supportedProtocols = cms.untracked.vstring('rfio')
        #filter = cms.untracked.string('\\.lhe$')
)

from Configuration.Generator.PythiaUESettings_cfi import *

generator = cms.EDProducer("Pythia6HadronizerFilter",
    pythiaHepMCVerbosity = cms.untracked.bool(True),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    comEnergy = cms.double(10000.0),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('MSEL=0         ! User defined processes', 
                        'PMAS(5,1)=4.4   ! b quark mass',
                        'PMAS(6,1)=172.4 ! t quark mass',
			'MSTJ(1)=1       ! Fragmentation/hadronization on or off',
			'MSTP(61)=1      ! Parton showering on or off'),
        # This is a vector of ParameterSet names to be read, in this order
        parameterSets = cms.vstring('pythiaUESettings', 
            'processParameters')
    )
)

genParticlesjgj = cms.EDProducer("GenParticleProducer",
    saveBarCodes = cms.untracked.bool(True),
    src = cms.InputTag("generator"),
    abortOnUnknownPDGCode = cms.untracked.bool(True)
)

genJetParticlesjgj = cms.EDFilter("InputGenJetsParticleSelector",
    src = cms.InputTag("genParticlesjgj"),
    ignoreParticleIDs = cms.vuint32(1000022, 2000012, 2000014, 2000016, 1000039, 
        5000039, 4000012, 9900012, 9900014, 9900016, 
        39),
    partonicFinalState = cms.bool(False),
    excludeResonances = cms.bool(True),
    excludeFromResonancePids = cms.vuint32(12, 13, 14, 16),
    tausAsJets = cms.bool(False)
)

GenJetParametersjgj = cms.PSet(
    src            = cms.InputTag("genJetParticlesjgj"),
    verbose        = cms.untracked.bool(False),
    jetPtMin       = cms.double(5.0),
    inputEtMin     = cms.double(0.0),
    jetType        = cms.untracked.string('GenJet'),
    inputEMin      = cms.double(0.0)
)

FastjetNoPUjgj = cms.PSet(
    Active_Area_Repeats = cms.int32(0),
    GhostArea = cms.double(1.0),
    Ghost_EtaMax = cms.double(0.0),
    UE_Subtraction = cms.string('no')
)

KtJetParametersjgj = cms.PSet(
     #possible Strategies: "Best","N2Plain","N2Tiled","N2MinHeapTiled","NlnN","NlnNCam"
     Strategy = cms.string('Best')
)

kt4GenJetsjgj = cms.EDProducer("KtJetProducer",
    GenJetParametersjgj,
    FastjetNoPUjgj,
    KtJetParametersjgj,
    alias = cms.untracked.string("KT4GenJetjgj"),
    FJ_ktRParam = cms.double(0.4)
)

kt6GenJetsjgj = cms.EDProducer("KtJetProducer",
    GenJetParametersjgj,
    FastjetNoPUjgj,
    KtJetParametersjgj,
    alias = cms.untracked.string("KT6GenJetjgj"),
    FJ_ktRParam = cms.double(0.6)
)

SISConeJetParametersjgj = cms.PSet(
     protojetPtMin = cms.double(0.0),
     
     coneOverlapThreshold = cms.double(0.75),
     caching = cms.bool(False),
     maxPasses = cms.int32(0),
     splitMergeScale = cms.string('pttilde'),
     maxInputSize = cms.uint32(1000)
)


sisCone5GenJetsjgj = cms.EDProducer("SISConeJetProducer",
     GenJetParametersjgj,
     SISConeJetParametersjgj,
     FastjetNoPUjgj,
     alias = cms.untracked.string('SISC5GenJetjgj'),
     coneRadius = cms.double(0.5)
)




#genJetParticlesjgj = cms.Sequence(genParticlesForJetsjgj)
#recoGenJetsjgj = cms.Sequence(sisCone5GenJetsjgj + kt4GenJetsjgj)

#genJetjgj = cms.Sequence(genParticlesjgj + (genJetParticlesjgj + recoGenJetsjgj))

jgjFilter = cms.EDFilter('JGJFilter')

ProductionFilterSequence = cms.Sequence(generator*(genParticlesjgj+(genJetParticlesjgj+(kt4GenJetsjgj+kt6GenJetsjgj+sisCone5GenJetsjgj)))*jgjFilter)
