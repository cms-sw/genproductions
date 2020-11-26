
import FWCore.ParameterSet.Config as cms

configurationMetadata = cms.untracked.PSet(
        version = cms.untracked.string('$Revision: 1.2 $'),
        name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/UserCode/AFanfani/X3872Analysis/Configuration/GenProduction/python/EightTeV/PYTHIA6_X3872ToJPsi_8TeV_Singlet_cff.py,v $'),
        annotation = cms.untracked.string(' 8TeV, D6T tune')
)

from Configuration.Generator.PythiaUEZ2starSettings_cfi import *

generator = cms.EDFilter("Pythia6GeneratorFilter",
    pythiaPylistVerbosity = cms.untracked.int32(0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(8000.0),
    crossSection = cms.untracked.double(3121000.0),
    filterEfficiency = cms.untracked.double(0.0097),
    maxEventsToPrint = cms.untracked.int32(0),
    ExternalDecays = cms.PSet(
        EvtGen = cms.untracked.PSet(
             operates_on_particles = cms.vint32( 0 ), # 0 (zero) means default list (hardcoded)
                                                      # you can put here the list of particles (PDG IDs)
                                                      # that you want decayed by EvtGen
             use_default_decay = cms.untracked.bool(False),
             decay_table = cms.FileInPath('GeneratorInterface/ExternalDecays/data/DECAY_NOLONGLIFE.DEC'),
             particle_property_file = cms.FileInPath('GeneratorInterface/ExternalDecays/data/evt.pdl'),
             user_decay_file = cms.FileInPath('GeneratorInterface/ExternalDecays/data/Onia_mumu_withX3872.dec'),
             list_forced_decays = cms.vstring('myX3872'),
        ),
        parameterSets = cms.vstring('EvtGen')
    ),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('MSEL=0          ! Free choice',
            'MSUB(88) = 1  !Activate gg->chi_1_c+g',
            'PMAS(289,1) = 3.87156 !Change chi_1_c mass to X3872',
            'PMAS(289,2) = 0.0023 !Change chi_c1 width to X3872' ,


            'MSTP(142)=2      ! turns on the PYEVWT Pt re-weighting routine',

            'PARJ(13)=0.750   ! probability that a c or b meson has S=1',
            'PARJ(14)=0.162   ! probability that a meson with S=0 is produced with L=1, J=1',
            'PARJ(15)=0.018   ! probability that a meson with S=1 is produced with L=1, J=0',
            'PARJ(16)=0.054   ! probability that a meson with S=1 is produced with L=1, J=1',
            'MSTP(145)=0      ! choice of polarization',
            'MSTP(146)=0      ! choice of polarization frame ONLY when mstp(145)=1',
            'MSTP(147)=0      ! particular helicity or density matrix component when mstp(145)=1',
            'MSTP(148)=1      ! possibility to allow for final-state shower evolution, extreme case !',
            'MSTP(149)=1      ! if mstp(148)=1, it determines the kinematics of the QQ~3S1(8)->QQ~3S1(8)+g branching'),
        # This is a vector of ParameterSet names to be read, in this order 
        parameterSets = cms.vstring('pythiaUESettings',
            'processParameters',
            'CSAParameters'),
        CSAParameters = cms.vstring('CSAMODE = 6     ! cross-section reweighted quarkonia')
    )
)


oniafilter = cms.EDFilter("PythiaFilter",
                          Status = cms.untracked.int32(2),   
                          MaxRapidity = cms.untracked.double(2.2),
                          MinRapidity = cms.untracked.double(-2.2),
                          MinPt = cms.untracked.double(10.0),
                          ParticleID = cms.untracked.int32(20443) ## X(3872) as chic
                          )

mumugenfilter = cms.EDFilter("MCParticlePairFilter",
    Status = cms.untracked.vint32(1, 1),
#    MinPt = cms.untracked.vdouble(2.5, 2.5),
    MinPt = cms.untracked.vdouble(3., 3.),
    MaxEta = cms.untracked.vdouble(2.2, 2.2),
    MinEta = cms.untracked.vdouble(-2.2, -2.2),
    ParticleCharge = cms.untracked.int32(-1),
    ParticleID1 = cms.untracked.vint32(13),
    ParticleID2 = cms.untracked.vint32(13)
)


ProductionFilterSequence = cms.Sequence(generator*oniafilter*mumugenfilter)


