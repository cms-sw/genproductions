import FWCore.ParameterSet.Config as cms

#source = cms.Source("EmptySource")

from Configuration.Generator.PythiaUEZ2Settings_cfi import *

generator = cms.EDFilter("Pythia6GeneratorFilter",
    comEnergy = cms.double(8000.0),
    crossSection = cms.untracked.double(4.9),
    filterEfficiency = cms.untracked.double(0.00245),
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
             user_decay_file = cms.FileInPath('GeneratorInterface/ExternalDecays/data/Bd_mumu.dec'),
             list_forced_decays = cms.vstring('MyB0',
                                              'Myanti-B0'),
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
## MuFromBd = cms.EDFilter("PythiaFilter",
##     MaxEta = cms.untracked.double(2.5),
##     Status = cms.untracked.int32(1),
##     MinEta = cms.untracked.double(-2.5),
##     MotherID = cms.untracked.int32(511),
##     ParticleID = cms.untracked.int32(13)
## )
## mumugenfilter = cms.EDFilter("MCParticlePairFilter",
##     MaxEta = cms.untracked.vdouble(2.5, 2.5),
##     Status = cms.untracked.vint32(1, 1),
##     MinEta = cms.untracked.vdouble(-2.5, -2.5),
##     ParticleID1 = cms.untracked.vint32(-13, 13),
##     ParticleID2 = cms.untracked.vint32(-13, 13)
## )

bdfilter = cms.EDFilter("PythiaFilter",
    ParticleID = cms.untracked.int32(511)
)

mumugenfilter = cms.EDFilter("MCParticlePairFilter",
    Status = cms.untracked.vint32(1, 1),
    MinP = cms.untracked.vdouble(2.5, 2.5),
    MaxEta = cms.untracked.vdouble(2.5, 2.5),
    MinEta = cms.untracked.vdouble(-2.5, -2.5),
    ParticleCharge = cms.untracked.int32(-1),
    ParticleID1 = cms.untracked.vint32(13),
    ParticleID2 = cms.untracked.vint32(13)
)

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string
    ('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_Bd2MuMu_TuneZ2_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('Bd -> mu mu at 7TeV')
    )

## ProductionFilterSequence = cms.Sequence(generator*MuFromBd*mumugenfilter)
ProductionFilterSequence = cms.Sequence(generator*bdfilter*mumugenfilter)
