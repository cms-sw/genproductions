import FWCore.ParameterSet.Config as cms

from Configuration.Generator.PythiaUEZ2starSettings_cfi import *

bfilter = cms.EDFilter(
        "PythiaFilter",
        MaxEta = cms.untracked.double(9999.),
        MinEta = cms.untracked.double(-9999.),
        ParticleID = cms.untracked.int32(521)
        )

generator = cms.EDFilter(
    "Pythia6GeneratorFilter",
    ExternalDecays = cms.PSet(
        EvtGen = cms.untracked.PSet(
            use_default_decay = cms.untracked.bool(False),
            decay_table = cms.FileInPath('GeneratorInterface/ExternalDecays/data/DECAY_NOLONGLIFE.DEC'),
            particle_property_file = cms.FileInPath('GeneratorInterface/ExternalDecays/data/evtJpsiKKKY.pdl'),
            user_decay_file = cms.FileInPath('GeneratorInterface/ExternalDecays/data/Bu_Y4727K_JpsiPhi_mumuKK.dec'),
            list_forced_decays = cms.vstring('MyB+',
                                             'MyB-'),
            operates_on_particles = cms.vint32(0)
            ),
        parameterSets = cms.vstring('EvtGen')
        ),
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(6e-05),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(8000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        bbbarSettings = cms.vstring('MSEL = 1 !'),
        parameterSets = cms.vstring(
            'pythiaUESettings', 
            'bbbarSettings')
        )
    )

ProductionFilterSequence = cms.Sequence(generator * bfilter)
