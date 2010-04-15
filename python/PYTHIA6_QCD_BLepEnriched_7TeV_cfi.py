import FWCore.ParameterSet.Config as cms

source = cms.Source("EmptySource")

from Configuration.Generator.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    comEnergy = cms.double(7000.0),
    ExternalDecays = cms.PSet(
        EvtGen = cms.untracked.PSet(
            use_default_decay = cms.untracked.bool(True),
            decay_table = cms.FileInPath('GeneratorInterface/ExternalDecays/data/DECAY_NOLONGLIFE.DEC'),
            particle_property_file = cms.FileInPath('GeneratorInterface/ExternalDecays/data/evt.pdl'),
            user_decay_file = cms.FileInPath('GeneratorInterface/ExternalDecays/data/Validation.dec'),
            list_forced_decays = cms.vstring(), 
            operates_on_particles = cms.vint32(0)
        ),
        parameterSets = cms.vstring('EvtGen')
    ),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        mySettings = cms.vstring('MSEL = 1'),
        parameterSets = cms.vstring('pythiaUESettings','mySettings')
    )
)

bFilter = cms.EDFilter("PythiaFilter",
    ParticleID = cms.untracked.int32(5)
)

lepFilter = cms.EDFilter("MCSingleParticleFilter",
    Status = cms.untracked.vint32(1,1,1,1),
    MinPt = cms.untracked.vdouble(5.0,5.0,5.0,5.0),
    MaxEta = cms.untracked.vdouble( 2.4, 2.4, 2.4, 2.4),
    MinEta = cms.untracked.vdouble(-2.4,-2.4,-2.4,-2.4),
    ParticleID = cms.untracked.vint32(11,-11,13,-13)
)

ProductionFilterSequence = cms.Sequence(generator * bFilter * lepFilter) 
