import FWCore.ParameterSet.Config as cms

#source = cms.Source("EmptySource")

from Configuration.Generator.PythiaUEZ2starSettings_cfi import *

generator = cms.EDFilter(
    "Pythia6GeneratorFilter",
    comEnergy = cms.double(8000.0),
    crossSection = cms.untracked.double(59508.),
    filterEfficiency = cms.untracked.double(0.000151),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(0),
    ExternalDecays = cms.PSet(
        EvtGen = cms.untracked.PSet(
             operates_on_particles = cms.vint32( 0 ), # 0 (zero) means default list (hardcoded)
                                                      # you can put here the list of particles (PDG IDs)
                                                      # that you want decayed by EvtGen
             use_default_decay = cms.untracked.bool(False),
             decay_table = cms.FileInPath('GeneratorInterface/ExternalDecays/data/DECAY_NOLONGLIFE.DEC'),
             particle_property_file = cms.FileInPath('GeneratorInterface/ExternalDecays/data/evt.pdl'),
             user_decay_file = cms.FileInPath('GeneratorInterface/ExternalDecays/data/Bs_pipi.dec'),
             list_forced_decays = cms.vstring('MyB_s0',
                                              'Myanti-B_s0'),
        ),
        parameterSets = cms.vstring('EvtGen')
    ),

    
    PythiaParameters = cms.PSet(
    pythiaUESettingsBlock,
         bbbarSettings = cms.vstring('MSEL = 1'), 
        # This is a vector of ParameterSet names to be read, in this order
        parameterSets = cms.vstring(
             'pythiaUESettings',
             'bbbarSettings')
       
    )
    )

bfilter = cms.EDFilter(
        "PythiaFilter",
        MaxEta = cms.untracked.double(9999.),
        MinEta = cms.untracked.double(-9999.),
        ParticleID = cms.untracked.int32(531)
        )

decayfilter = cms.EDFilter(
        "PythiaDauVFilter",
	verbose         = cms.untracked.int32(0), 
	NumberDaughters = cms.untracked.int32(2), 
	ParticleID      = cms.untracked.int32(531),  
        DaughterIDs     = cms.untracked.vint32(211, -211),
	MinPt           = cms.untracked.vdouble(3.5, 3.5), 
	MinEta          = cms.untracked.vdouble(-2.5, -2.5), 
	MaxEta          = cms.untracked.vdouble( 2.5,  2.5)
        )

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string
    ('$Source: /local/projects/CMSSW/rep/CMSSW/Configuration/GenProduction/python/PYTHIA6_Bs2PiPi_TuneZ2_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('Bs -> pi+ pi- at 7TeV')
    )

ProductionFilterSequence = cms.Sequence(generator*bfilter*decayfilter)
