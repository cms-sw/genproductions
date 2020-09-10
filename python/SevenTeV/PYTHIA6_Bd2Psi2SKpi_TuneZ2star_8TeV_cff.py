import FWCore.ParameterSet.Config as cms

#source = cms.Source("EmptySource")

from Configuration.Generator.PythiaUEZ2starSettings_cfi import *

generator = cms.EDFilter(
    "Pythia6GeneratorFilter",
    comEnergy = cms.double(8000.0),
    crossSection = cms.untracked.double(0.0),
    filterEfficiency = cms.untracked.double(1.0),
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
             user_decay_file = cms.FileInPath('GeneratorInterface/ExternalDecays/data/Bd_Psi2SKpi.dec'),
             list_forced_decays = cms.vstring('MyB0',
                                              'Myanti-B0'),
        ),
        parameterSets = cms.vstring('EvtGen')
    ),

    
    PythiaParameters = cms.PSet(
    pythiaUESettingsBlock,
         bbbarSettings = cms.vstring('MSEL=5'), 
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
        ParticleID = cms.untracked.int32(511)
        )

decayfilter = cms.EDFilter(
        "PythiaDauVFilter",
	verbose         = cms.untracked.int32(0), 
	NumberDaughters = cms.untracked.int32(3), 
	ParticleID      = cms.untracked.int32(511),  
        DaughterIDs     = cms.untracked.vint32(100443, 321, -211),
	MinPt           = cms.untracked.vdouble(-1.0, 0.4, 0.4), 
	MinEta          = cms.untracked.vdouble(-9999., -2.5, -2.5), 
	MaxEta          = cms.untracked.vdouble( 9999.,  2.5,  2.5)
        )

psifilter = cms.EDFilter(
        "PythiaDauVFilter",
        verbose         = cms.untracked.int32(0), 
        NumberDaughters = cms.untracked.int32(2), 
        MotherID        = cms.untracked.int32(511),  
        ParticleID      = cms.untracked.int32(100443),  
        DaughterIDs     = cms.untracked.vint32(13, -13),
        MinPt           = cms.untracked.vdouble(1.0, 1.0), 
        MinEta          = cms.untracked.vdouble(-2.5, -2.5), 
        MaxEta          = cms.untracked.vdouble( 2.5,  2.5)
        )


configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    #name = cms.untracked.string('$Source: /local/reps/CMSSW/CMSSW/Configuration/GenProduction/python/EightTeV/PYTHIA6_Bd2Psi2SKpi_TuneZ2star_8TeV_cff.py,v $'),
    name = cms.untracked.string('$Source: /afs/cern.ch/user/l/lecriste/sanjay_code/CMSSW_5_3_22/src/Configuration/GenProduction/python/PYTHIA6_Bd2Psi2SKpi_TuneZ2star_8TeV_cff.py,v $'),    
    annotation = cms.untracked.string('B0 -> Psi(2S)K+pi- at 8TeV')
    )

ProductionFilterSequence = cms.Sequence(generator*bfilter*decayfilter*psifilter)
