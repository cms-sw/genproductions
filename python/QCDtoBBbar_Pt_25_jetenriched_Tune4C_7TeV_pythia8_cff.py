import FWCore.ParameterSet.Config as cms

generator = cms.EDFilter("Pythia8GeneratorFilter",
    crossSection = cms.untracked.double(540000),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(0.087),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    PythiaParameters = cms.PSet(
        processParameters = cms.vstring(
            'Main:timesAllowErrors    = 10000', 

            'ParticleDecays:limitTau0 = on', 
            'ParticleDecays:tauMax = 10', 

            'HardQCD:gg2bbbar = on',
            'HardQCD:qqbar2bbbar = on',

            'PhaseSpace:pTHatMin = 25.',

            'SecondHard:generate = off',
            'SecondHard:TwoJets = off', 

            'PartonLevel:MI = on',

            'Tune:pp 5'    
	    ),                  
        parameterSets = cms.vstring('processParameters')
    )
)

genParticlesRe = cms.EDProducer("GenParticleProducer",
                                        saveBarCodes = cms.untracked.bool(True),
                                        src = cms.InputTag("generator"),
                                        abortOnUnknownPDGCode = cms.untracked.bool(False)
                                      )

genJetParticlesRe = cms.EDProducer("InputGenJetsParticleSelector",
                                          src = cms.InputTag("genParticlesRe"),
                                          ignoreParticleIDs = cms.vuint32(1000022, 2000012, 2000014, 2000016, 1000039,
                                                                          5000039, 4000012, 9900012, 9900014, 9900016,
                                                                          39),
                                          partonicFinalState = cms.bool(False),
                                          excludeResonances = cms.bool(True),
                                          excludeFromResonancePids = cms.vuint32(12, 13, 14, 16),
                                          tausAsJets = cms.bool(False)
                                         )

GenJetParametersRe = cms.PSet(
                             src            = cms.InputTag("genJetParticlesRe"),
                             verbose        = cms.untracked.bool(False),
                             jetPtMin       = cms.double(5.0),
                             inputEtMin     = cms.double(0.0),
                             jetType        = cms.string('GenJet'),
                             inputEMin      = cms.double(0.0),
                             srcPVs         = cms.InputTag(''),
                             doPVCorrection = cms.bool(False),
                        # pileup with offset correction
                             doPUOffsetCorr = cms.bool(False),
                        # if pileup is false, these are not read:
                             nSigmaPU = cms.double(1.0),
                             radiusPU = cms.double(0.5),
                        # fastjet-style pileup
                             doAreaFastjet  = cms.bool(False),
                             doRhoFastjet   = cms.bool(False),
                        # if doPU is false, these are not read:
                             Active_Area_Repeats = cms.int32(5),
                             GhostArea = cms.double(0.01),
                             Ghost_EtaMax = cms.double(6.0),
                             Rho_EtaMax = cms.double(4.5)
                            )
from RecoJets.JetProducers.AnomalousCellParameters_cfi import *

ak5GenJetsRe = cms.EDProducer("FastjetJetProducer",
                                     GenJetParametersRe,
                                     AnomalousCellParameters,
                                     jetAlgorithm = cms.string("AntiKt"),
                                     rParam       = cms.double(0.5)
                                    )

NJetsMC  =  cms.EDFilter('NJetsMC',
                GenTag = cms.untracked.InputTag("ak5GenJetsRe"),
		Njets  = cms.int32(4),
                MinPt  = cms.double(15.)
                          )

ProductionFilterSequence = cms.Sequence(generator*(genParticlesRe+(genJetParticlesRe+ak5GenJetsRe))*NJetsMC)

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs/CMSSW/CMSSW/Configuration/GenProduction/python/QCDtoBBbar_Pt_25_jetenriched_Tune4C_7TeV_pythia8_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA8 QCD to BBbar, pThat > 25GeV at sqrt(s) = 7TeV, Tune 4C MPI default, 4 genjets filtering')
)
