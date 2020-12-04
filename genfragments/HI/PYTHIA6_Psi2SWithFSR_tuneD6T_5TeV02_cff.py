import FWCore.ParameterSet.Config as cms

configurationMetadata = cms.untracked.PSet(
        version = cms.untracked.string('$Revision: 1.1 $'),
        name = cms.untracked.string('$Source: /local/reps/CMSSW/CMSSW/Configuration/GenProduction/python/EightTeV/PYTHIA6_Psi2SWithFSR_tuneD6T_8TeV_cff.py,v $'),
        annotation = cms.untracked.string('Winter13: Pythia6 generation of prompt Psi2S, 5.023TeV, D6T tune')
)

from Configuration.Generator.PythiaUESettings_cfi import *

generator = cms.EDFilter("Pythia6GeneratorFilter",
    pythiaPylistVerbosity = cms.untracked.int32(0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(5023.0),
    crossSection = cms.untracked.double(45915.),
    filterEfficiency = cms.untracked.double(0.0271),
    maxEventsToPrint = cms.untracked.int32(0),
    ExternalDecays = cms.PSet(
        EvtGen = cms.untracked.PSet(
            use_internal_pythia = cms.untracked.bool(False),
            operates_on_particles = cms.vint32( 100443 ), # 0 (zero) means default list (hardcoded)
                                                          # you can put here the list of particles (PDG IDs)
                                                          # that you want decayed by EvtGen
            use_default_decay = cms.untracked.bool(False),
            decay_table = cms.FileInPath('GeneratorInterface/ExternalDecays/data/DECAY_NOLONGLIFE.DEC'),
            particle_property_file = cms.FileInPath('GeneratorInterface/ExternalDecays/data/evt.pdl'),
            user_decay_file = cms.FileInPath('GeneratorInterface/ExternalDecays/data/Onia_mumu.dec'),
            list_forced_decays = cms.vstring('Mypsi(2S)'),
        ),
        parameterSets = cms.vstring('EvtGen')
    ),                             
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('MSEL=61          ! Quarkonia', 
            'KFPR(421,1)  = 100443    ! change 421 to Psi(2S) + g', 
            'PMAS(362,1)  = 3.70000   ! change cc~ mass larger than Psi(2S) 3.68600', 
            'PMAS(363,1)  = 3.70000   ! change cc~ mass larger than Psi(2S) 3.68600', 
            'PMAS(364,1)  = 3.70000   ! change cc~ mass larger than Psi(2S) 3.68600', 
            'KFDP(4211,1) = 100443    ! cc~ -> Psi(2S)', 
            'KFDP(4212,1) = 100443    ! cc~ -> Psi(2S)', 
            'KFDP(4213,1) = 100443    ! cc~ -> Psi(2S)', 
            'PARP(141)=0.76   ! New values for COM matrix elements', 
            'PARP(142)=0.0050 ! New values for COM matrix elements', 
            'PARP(143)=0.0042 ! New values for COM matrix elements', 
            'PARP(144)=0.0042 ! New values for COM matrix elements', 
            'PARP(145)=0      ! New values for COM matrix elements', 
            'MDME(1567,1) = 0 ! 0.008300    e-              e+', 
            'MDME(1568,1) = 1 ! 0.008300    mu-             mu+', 
            'MDME(1569,1) = 0 ! 0.186600    rndmflav        rndmflavbar', 
            'MDME(1570,1) = 0 ! 0.324000    J/psi           pi+             pi-', 
            'MDME(1571,1) = 0 ! 0.184000    J/psi           pi0             pi0', 
            'MDME(1572,1) = 0 ! 0.027000    J/psi           eta', 
            'MDME(1573,1) = 0 ! 0.001000    J/psi           pi0', 
            'MDME(1574,1) = 0 ! 0.093000    chi_0c          gamma', 
            'MDME(1575,1) = 0 ! 0.087000    chi_1c          gamma', 
            'MDME(1576,1) = 0 ! 0.078000    chi_2c          gamma', 
            'MDME(1577,1) = 0 ! 0.002800    eta_c           gamma', 
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

oniafilter = cms.EDFilter("PythiaFilter",
    Status = cms.untracked.int32(2),
    MaxEta = cms.untracked.double(1e100),
    MinEta = cms.untracked.double(-1e100),
    MinPt = cms.untracked.double(0.0),
    ParticleID = cms.untracked.int32(100443)
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

ProductionFilterSequence = cms.Sequence(generator*oniafilter*mumugenfilter)

