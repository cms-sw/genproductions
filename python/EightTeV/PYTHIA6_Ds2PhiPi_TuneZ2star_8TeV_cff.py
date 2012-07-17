import FWCore.ParameterSet.Config as cms

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
            user_decay_file = cms.FileInPath('GeneratorInterface/ExternalDecays/data/Ds_phipi_mumupi.dec'),
            list_forced_decays = cms.vstring('MyD_s+','MyD_s-')
            ),
        parameterSets = cms.vstring('EvtGen')
        )			 
    
    )


configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.3 $'),
    annotation = cms.untracked.string('Ds -> Phi Pi at 8 TeV'),
    name = cms.untracked.string
    ('$Source: /local/reps/CMSSW/CMSSW/Configuration/GenProduction/python/EightTeV/PYTHIA6_Ds2PhiPi_TuneZ2star_8TeV_cff.py,v $')
    )

# filter to select events with a Ds
Dfilter = cms.EDFilter(
    "PythiaFilter",
    Status = cms.untracked.int32(2),
    MaxEta = cms.untracked.double(3),
    MinEta = cms.untracked.double(-3),
    MinPt = cms.untracked.double(5.0),
    ParticleID = cms.untracked.int32(431)  #D_s 
    )

ProductionFilterSequence = cms.Sequence(generator*Dfilter)

