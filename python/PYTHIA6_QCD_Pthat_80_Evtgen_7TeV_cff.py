import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *

source = cms.Source("EmptySource")
from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(1),
crossSection = cms.untracked.double(9.140E+5),
    comEnergy = cms.double(7000.0),

    ExternalDecays = cms.PSet(
    EvtGen = cms.untracked.PSet(
             operates_on_particles = cms.vint32( 0 ), # 0 (zero) means default list (hardcoded)
                                                      # you can put here the list of particles (PDG IDs)
                                                      # that you want decayed by EvtGen
	     use_default_decay = cms.untracked.bool(True),
             decay_table = cms.FileInPath('GeneratorInterface/ExternalDecays/data/DECAY_NOLONGLIFE.DEC'),
             particle_property_file = cms.FileInPath('GeneratorInterface/ExternalDecays/data/evt.pdl'),
             user_decay_file = cms.FileInPath('GeneratorInterface/ExternalDecays/data/Validation.dec'),
             list_forced_decays = cms.vstring()
             ),
        parameterSets = cms.vstring('EvtGen')
    ),


                         
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring(
    'MSEL=1           ! User defined processes', 
            'CKIN(3)=80.      ! minimum pt hat for hard interactions'
    ),
        # This is a vector of ParameterSet names to be read, in this order
        parameterSets = cms.vstring('pythiaUESettings', 
                                    'processParameters')
    )
)



configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_QCD_Pthat_80_Evtgen_10TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6-QCD at 7TeV, pthat>80, with EVTGEN')
)

ProductionFilterSequence = cms.Sequence(generator)
