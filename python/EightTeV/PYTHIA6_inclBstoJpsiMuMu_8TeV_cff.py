import FWCore.ParameterSet.Config as cms

configurationMetadata = cms.untracked.PSet(
        version = cms.untracked.string('$Revision: 1.9 $'),
        name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/Attic/PYTHIA6_inclBstoJpsiMuMuTight_7TeV_cff.py,v $'),
        annotation = cms.untracked.string('Summer09: Pythia6+EvtGen generation of Bs->Jpsi->MuMu, 7TeV, D6T tune')
)

from Configuration.Generator.PythiaUEZ2starSettings_cfi import *

generator = cms.EDFilter("Pythia6GeneratorFilter",
    pythiaPylistVerbosity = cms.untracked.int32(0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(8000.0),
#   BF(Bs->JpsiX) * BF(JPsi->mumu) = 0.0168 * 0.0593
    crossSection = cms.untracked.double(49403541.6),
    filterEfficiency = cms.untracked.double(0.0001004),
    maxEventsToPrint = cms.untracked.int32(0),
    ExternalDecays = cms.PSet(
        EvtGen = cms.untracked.PSet(
             operates_on_particles = cms.vint32( 0 ), # 0 (zero) means default list (hardcoded)
                                                      # you can put here the list of particles (PDG IDs)
                                                      # that you want decayed by EvtGen
             use_default_decay = cms.untracked.bool(False),
             decay_table = cms.FileInPath('GeneratorInterface/ExternalDecays/data/DECAY_NOLONGLIFE.DEC'),
             #decay_table = cms.FileInPath('GeneratorInterface/ExternalDecays/data/DECAY.DEC'),
             particle_property_file = cms.FileInPath('GeneratorInterface/ExternalDecays/data/evt.pdl'),
             #user_decay_file = cms.FileInPath('GeneratorInterface/ExternalDecays/data/Validation.dec'),
             user_decay_file = cms.FileInPath('GeneratorInterface/ExternalDecays/data/incl_BtoJpsi_mumu.dec'),
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

bsfilter = cms.EDFilter("PythiaFilter",
    ParticleID = cms.untracked.int32(531)
)

oniafilter = cms.EDFilter("MCSingleParticleFilter",
    Status = cms.untracked.vint32(2, 2),
    MaxEta = cms.untracked.vdouble(1000.0, 1000.0),
    MinEta = cms.untracked.vdouble(-1000.0, -1000.0),
    MinPt = cms.untracked.vdouble(0.0, 0.0),
    ParticleID = cms.untracked.vint32(443, 100443)
)

mumugenfilter = cms.EDFilter("MCParticlePairFilter",
    Status = cms.untracked.vint32(1, 1),
    MinPt = cms.untracked.vdouble(3., 3.),
    MaxEta = cms.untracked.vdouble(2.2, 2.2),
    MinEta = cms.untracked.vdouble(-2.2, -2.2),
    ParticleCharge = cms.untracked.int32(-1),
    ParticleID1 = cms.untracked.vint32(13),
    ParticleID2 = cms.untracked.vint32(13)
)


ProductionFilterSequence = cms.Sequence(generator*bsfilter*oniafilter*mumugenfilter)


