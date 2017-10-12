import FWCore.ParameterSet.Config as cms

#source = cms.Source("EmptySource")

from Configuration.Generator.PythiaUEZ2starSettings_cfi import *

generator = cms.EDFilter("Pythia6GeneratorFilter",
    ExternalDecays = cms.PSet(
        EvtGen = cms.untracked.PSet(
            use_default_decay = cms.untracked.bool(False),
            decay_table = cms.FileInPath('GeneratorInterface/ExternalDecays/data/DECAY_NOLONGLIFE.DEC'),
            particle_property_file = cms.FileInPath('GeneratorInterface/ExternalDecays/data/evt.pdl'),
            user_decay_file = cms.FileInPath('GeneratorInterface/ExternalDecays/data/Bs_Jpsif2_mumuKK.dec'),
            list_forced_decays = cms.vstring('MyB_s0', 
                'Myanti-B_s0'),
            operates_on_particles = cms.vint32(0)
        ),
        parameterSets = cms.vstring('EvtGen')
    ),
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(0.000422),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(8000.0),
    crossSection = cms.untracked.double(24100000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        bbbarSettings = cms.vstring('MSEL = 1'),
        parameterSets = cms.vstring('pythiaUESettings', 
            'bbbarSettings')
    )
)

f2filter = cms.EDFilter("PythiaFilter",
    MaxEta = cms.untracked.double(1000.0),
    Status = cms.untracked.int32(2),
    MinEta = cms.untracked.double(-1000.0),
    MinPt = cms.untracked.double(0.0),
    ParticleID = cms.untracked.int32(335)
)


oniafilter = cms.EDFilter("PythiaFilter",
    MaxEta = cms.untracked.double(1000.0),
    Status = cms.untracked.int32(2),
    MinEta = cms.untracked.double(-1000.0),
    MinPt = cms.untracked.double(0.0),
    ParticleID = cms.untracked.int32(443)
)


bsfilter = cms.EDFilter("PythiaFilter",
    ParticleID = cms.untracked.int32(531)
)


kkgenfilter = cms.EDFilter("MCParticlePairFilter",
    Status = cms.untracked.vint32(1, 1),
    MaxEta = cms.untracked.vdouble(2.5, 2.5),
    MinEta = cms.untracked.vdouble(-2.5, -2.5),
    ParticleCharge = cms.untracked.int32(-1),
    MinP = cms.untracked.vdouble(1.0, 1.0),
    ParticleID1 = cms.untracked.vint32(321),
    ParticleID2 = cms.untracked.vint32(321)
)

mumugenfilter = cms.EDFilter("MCParticlePairFilter",
    Status = cms.untracked.vint32(1, 1),
    MaxEta = cms.untracked.vdouble(2.2, 2.2),
    MinEta = cms.untracked.vdouble(-2.2, -2.2),
    ParticleCharge = cms.untracked.int32(-1),
    MinPt = cms.untracked.vdouble(2., 2.),
    ParticleID1 = cms.untracked.vint32(13),
    ParticleID2 = cms.untracked.vint32(13)
)

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string
    ('$Source: /local/reps/CMSSW/CMSSW/Configuration/GenProduction/python/EightTeV/PYTHIA6_Bs2Jpsif2_TuneZ2star_8TeV_cff.py,v $'),
    annotation = cms.untracked.string('Bs -> Jpsi Phi at 7TeV')
    )

ProductionFilterSequence = cms.Sequence(generator*bsfilter*oniafilter*f2filter*mumugenfilter*kkgenfilter)
