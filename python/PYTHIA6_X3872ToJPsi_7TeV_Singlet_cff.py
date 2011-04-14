# Auto generated configuration file
# using: 
# Revision: 1.182 
# Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/PyReleaseValidation/python/ConfigBuilder.py,v 
# with command line options: Configuration/GenProduction/python/PYTHIA6_Psi2SToJPsi_7TeV_cff.py -s GEN,SIM,DIGI,L1,DIGI2RAW,RAW2DIGI,RECO --eventcontent FEVTSIM --conditions START37_V5::All --datatier GEN-SIM-RAW-RECO -n 10 --no_exec
import FWCore.ParameterSet.Config as cms


configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    annotation = cms.untracked.string('September 2010: Pythia6 generation of prompt X3872, 7TeV, withEvtGen'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_X3872ToJPsi_7TeV_Singlet_cff.py,v $')
)

#from Configuration.GenProduction.PythiaUESettings_cfi import *
from Configuration.Generator.PythiaUEZ2Settings_cfi import * #MSTP(81)=1, 21 probably wrong on CVS

# Input source
source = cms.Source("EmptySource")



generator = cms.EDFilter("Pythia6GeneratorFilter",
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(0.123),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    crossSection = cms.untracked.double(3688000.0),
    maxEventsToPrint = cms.untracked.int32(0),


    ExternalDecays = cms.PSet(
    EvtGen = cms.untracked.PSet(
         operates_on_particles = cms.vint32(0), # 0 (zero) means default list (hardcoded) 20443
                                                  # you can put here the list of particles (PDG IDs)
                                                  # that you want decayed by EvtGen
         use_default_decay = cms.untracked.bool(False),
         decay_table = cms.FileInPath('GeneratorInterface/ExternalDecays/data/DECAY_NOLONGLIFE.DEC'),
         #particle_property_file = cms.FileInPath('MCGenerator/evt2.pdl'),
         particle_property_file = cms.FileInPath('GeneratorInterface/ExternalDecays/data/evt.pdl'),
         #user_decay_file = cms.FileInPath('MCGenerator/X3872.dec'),
         user_decay_file = cms.FileInPath('GeneratorInterface/ExternalDecays/data/Onia_mumu.dec'),
         list_forced_decays = cms.vstring('myX3872'),
     ),
        parameterSets = cms.vstring('EvtGen')
    ),

    
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('MSEL=0          ! Free choice',
            'MSUB(88) = 1  !Activate gg->chi_1_c+g',
            'PMAS(289,1) = 3.872 !Change chi_1_c mass to X3872',
            'PMAS(289,2) = 0.003 !Change chi_c1 width to X3872' ,

                                        
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
        parameterSets = cms.vstring('pythiaUESettings', 
            'processParameters', 
            'CSAParameters'),
        CSAParameters = cms.vstring('CSAMODE = 6     ! cross-section reweighted quarkonia')
    )
)
mumugenfilter = cms.EDFilter("MCParticlePairFilter",
    Status = cms.untracked.vint32(1, 1),
    MaxEta = cms.untracked.vdouble(2.5, 2.5),
    MinEta = cms.untracked.vdouble(-2.5, -2.5),
    ParticleCharge = cms.untracked.int32(-1),
    MinP = cms.untracked.vdouble(2.5, 2.5),
    ParticleID1 = cms.untracked.vint32(13),
    ParticleID2 = cms.untracked.vint32(13)
)


oniafilter = cms.EDFilter("PythiaFilter",
    Status = cms.untracked.int32(2),
    MaxEta = cms.untracked.double(1000.0),
    MinEta = cms.untracked.double(-1000.0),
    MinPt = cms.untracked.double(0.0),
    ParticleID = cms.untracked.int32(20443)
)

ProductionFilterSequence = cms.Sequence(generator*oniafilter*mumugenfilter)

