import FWCore.ParameterSet.Config as cms

from Configuration.Generator.PythiaUESettings_cfi import *
configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/Spring08Production/data/TkDPG_Psi2S_10TeV.cff,v $'),
    annotation = cms.untracked.string('generation of prompt Psi2S COM+CSM')
)
source = cms.Source("PythiaSource",
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(0.0433),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.untracked.double(10000.0),
    crossSection = cms.untracked.double(2169.0),
    maxEventsToPrint = cms.untracked.int32(0),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('MSEL=61          ! Quarkonia', 
            'KFPR(421,1)  = 100443    ! change 421 to Psi(2S) + g', 
            'PMAS(362,1)  = 3.70000   ! change cc~ mass larger than Psi(2S) 3.68600', 
            'PMAS(363,1)  = 3.70000   ! change cc~ mass larger than Psi(2S) 3.68600', 
            'PMAS(364,1)  = 3.70000   ! change cc~ mass larger than Psi(2S) 3.68600', 
            'KFDP(4285,1) = 100443    ! cc~ -> Psi(2S)', 
            'KFDP(4286,1) = 100443    ! cc~ -> Psi(2S)', 
            'KFDP(4287,1) = 100443    ! cc~ -> Psi(2S)', 
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
    MaxEta = cms.untracked.double(1000.0),
    MinEta = cms.untracked.double(-1000.0),
    MinPt = cms.untracked.double(0.0),
    ParticleID = cms.untracked.int32(100443)
)

mumugenfilter = cms.EDFilter("MCParticlePairFilter",
    Status = cms.untracked.vint32(1, 1),
    MinPt = cms.untracked.vdouble(2.5, 2.5),
    MaxEta = cms.untracked.vdouble(2.5, 2.5),
    MinEta = cms.untracked.vdouble(-2.5, -2.5),
    ParticleCharge = cms.untracked.int32(-1),
    ParticleID1 = cms.untracked.vint32(13),
    ParticleID2 = cms.untracked.vint32(13)
)

ProductionFilterSequence = cms.Sequence(oniafilter*mumugenfilter)


