import FWCore.ParameterSet.Config as cms

#source = cms.Source("EmptySource")

from Configuration.Generator.PythiaUEZ2starSettings_cfi import *

generator = cms.EDFilter(
    "Pythia6GeneratorFilter",
    comEnergy = cms.double(8000.0),
    crossSection = cms.untracked.double(6744240),
    filterEfficiency = cms.untracked.double(0.00004),
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
             user_decay_file = cms.FileInPath('GeneratorInterface/ExternalDecays/data/LambdaB_pmunu.dec'),
             list_forced_decays = cms.vstring('MyLambda_b0',
                                              'Myanti-Lambda_b0'),
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
        ParticleID = cms.untracked.int32(5122)
        )

decayfilter = cms.EDFilter(
        "PythiaDauVFilter",
	verbose         = cms.untracked.int32(0), 
	NumberDaughters = cms.untracked.int32(3), 
	ParticleID      = cms.untracked.int32(5122),  
        DaughterIDs     = cms.untracked.vint32(2212, 13, -14),
	MinPt           = cms.untracked.vdouble(3.5, 3.5, -1.), 
	MinEta          = cms.untracked.vdouble(-2.5, -2.5, -9999.), 
	MaxEta          = cms.untracked.vdouble( 2.5,  2.5, 9999.)
        )


configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string
    ('$Source: /local/projects/CMSSW/rep/CMSSW/Configuration/GenProduction/python/PYTHIA6_LambdaB2PMuNu_TuneZ2_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('Lambda_b -> p+ mu- nu at 7TeV')
    )

ProductionFilterSequence = cms.Sequence(generator*bfilter*decayfilter)
