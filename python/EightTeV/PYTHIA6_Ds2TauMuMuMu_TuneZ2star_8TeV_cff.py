import FWCore.ParameterSet.Config as cms

source = cms.Source("EmptySource")

from Configuration.Generator.PythiaUEZ2starSettings_cfi import *

generator = cms.EDFilter(
    "Pythia6GeneratorFilter",
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(8000.0),
    crossSection = cms.untracked.double(71260000000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        bbbarSettings = cms.vstring('MSEL=1'),
        parameterSets = cms.vstring(
            'pythiaUESettings', 
            'bbbarSettings')
        ),
    ExternalDecays = cms.PSet(
	EvtGen = cms.untracked.PSet(
            operates_on_particles = cms.vint32(0), # 0=all
            use_default_decay = cms.untracked.bool(False),
            decay_table = cms.FileInPath('GeneratorInterface/ExternalDecays/data/DECAY_NOLONGLIFE.DEC'),
            particle_property_file = cms.FileInPath('GeneratorInterface/ExternalDecays/data/evt.pdl'),
            # user_decay_file = cms.FileInPath('GeneratorInterface/ExternalDecays/data/Ds_tau_mumumu.dec'),
            user_decay_file = cms.FileInPath('GeneratorInterface/ExternalDecays/data/Ds_tau_mumumu.dec'),
            list_forced_decays = cms.vstring('Mytau+','Mytau-','MyD_s+','MyD_s-')
            ),
	parameterSets = cms.vstring('EvtGen')
	),
    
    )


# filter to select events with a Ds
DsFilter = cms.EDFilter("PythiaFilter",
       Status = cms.untracked.int32(2),
       MaxEta = cms.untracked.double(3),
       MinEta = cms.untracked.double(-3),
       MinPt = cms.untracked.double(7),
       ParticleID = cms.untracked.int32(431)  #D_s 
   )




# ask 3 muons in the acceptance

genParticlesForFilter = cms.EDProducer(
    "GenParticleProducer",
    saveBarCodes = cms.untracked.bool(True),
    src = cms.InputTag("generator"),
    abortOnUnknownPDGCode = cms.untracked.bool(False)
    )


muonParticlesInAcc = cms.EDFilter(
    "GenParticleSelector",
    filter = cms.bool(False),
    src = cms.InputTag("genParticlesForFilter"),
    cut = cms.string('pt > 1. && abs(pdgId) == 13 && abs(eta) < 2.4'),
    stableOnly = cms.bool(True)
    )


threeMuonFilter = cms.EDFilter(
    "CandViewCountFilter",
    src = cms.InputTag("muonParticlesInAcc"),
    minNumber = cms.uint32(3)
    )


# Production Info
configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.2 $'),
    annotation = cms.untracked.string('MinBias_TuneZ2_7TeV_pythia6_cff.py nevts:1'),
    name = cms.untracked.string('PyReleaseValidation')
)

ProductionFilterSequence = cms.Sequence(generator * DsFilter * genParticlesForFilter * muonParticlesInAcc * threeMuonFilter)
