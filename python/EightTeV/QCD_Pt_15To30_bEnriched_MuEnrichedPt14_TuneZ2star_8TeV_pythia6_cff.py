import FWCore.ParameterSet.Config as cms

from Configuration.Generator.PythiaUEZ2starSettings_cfi import *

generator = cms.EDFilter("Pythia6GeneratorFilter",
    crossSection = cms.untracked.double(995500000.),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(0.0641),
    comEnergy = cms.double(8000.0),
    ExternalDecays = cms.PSet(
        EvtGen = cms.untracked.PSet(
             operates_on_particles = cms.vint32( 0 ), # 0 (zero) means default list (hardcoded)
                                                      # you can put here the list of particles (PDG IDs)
                                                      # that you want decayed by EvtGen
             use_default_decay = cms.untracked.bool(True),
             decay_table = cms.FileInPath('GeneratorInterface/ExternalDecays/data/DECAY_NOLONGLIFE.DEC'),
             particle_property_file = cms.FileInPath('GeneratorInterface/ExternalDecays/data/evt.pdl'),
             user_decay_file = cms.FileInPath('GeneratorInterface/ExternalDecays/data/DECAY_NOLONGLIFE.DEC'),
             list_forced_decays = cms.vstring(),
        ),
        parameterSets = cms.vstring('EvtGen')
    ),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('MSEL=1               ! QCD hight pT processes', 
	    'CKIN(3)=15.      ! minimum pt hat for hard interactions',
            'CKIN(4)=30.      ! maximum pt hat for hard interactions'),
        # This is a vector of ParameterSet names to be read, in this order
        parameterSets = cms.vstring('pythiaUESettings', 
            'processParameters')
    )
)

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /local/reps/CMSSW/CMSSW/Configuration/GenProduction/python/EightTeV/QCD_Pt_15To30_bEnriched_TuneZ2star_8TeV_pythia6_cff.py,v $'),
    annotation = cms.untracked.string('b-enriched QCD Pt [15,30] at 8 TeV')
)


######################
#####  filter b  #####
######################
bbFilter = cms.EDFilter("MCSingleParticleFilter",
   ParticleID = cms.untracked.vint32(5,-5),
   Status     = cms.untracked.vint32(2,2)
)  

#####################################
#####   filter mu from b-decay  #####
#####################################
muFilter = cms.EDFilter("MCSmartSingleParticleFilter",
    MinPt       = cms.untracked.vdouble(14., 14.),
    MinEta      = cms.untracked.vdouble(-2.5, -2.5),
    MaxEta      = cms.untracked.vdouble(2.5, 2.5),
    ParticleID  = cms.untracked.vint32(13, -13),
    Status      = cms.untracked.vint32(1, 1)
)

ProductionFilterSequence = cms.Sequence(generator*bbFilter*muFilter)
