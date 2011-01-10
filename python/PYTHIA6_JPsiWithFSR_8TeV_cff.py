import FWCore.ParameterSet.Config as cms

configurationMetadata = cms.untracked.PSet(
	version = cms.untracked.string('$Revision: 1.6 $'),
	name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_JPsiWithFSR_8TeV_cff.py,v $'),
	annotation = cms.untracked.string('Summer09: Pythia6 generation of prompt JPsi, 8TeV, Z2 tune')
)

from Configuration.Generator.PythiaUEZ2Settings_cfi import *

generator = cms.EDFilter("Pythia6GeneratorFilter",
    pythiaPylistVerbosity = cms.untracked.int32(0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(8000.0),
    crossSection = cms.untracked.double(8978000.0),
    # TrigReport     1    0     100000     100000          0          0 generator
    # TrigReport     1    0     100000      28995      71005          0 oniafilter
    # TrigReport     1    0      28995       5133      23862          0 mumugenfilter
    filterEfficiency = cms.untracked.double(0.0513),
    maxEventsToPrint = cms.untracked.int32(0),
    ExternalDecays = cms.PSet(
        EvtGen = cms.untracked.PSet(
            operates_on_particles = cms.vint32( 0 ), # 0 (zero) means default list (hardcoded)
                                                     # you can put here the list of particles (PDG IDs)
                                                     # that you want decayed by EvtGen
            use_default_decay = cms.untracked.bool(False),
            decay_table = cms.FileInPath('GeneratorInterface/ExternalDecays/data/DECAY_NOLONGLIFE.DEC'),
            particle_property_file = cms.FileInPath('GeneratorInterface/ExternalDecays/data/evt.pdl'),
            user_decay_file = cms.FileInPath('GeneratorInterface/ExternalDecays/data/Onia_mumu.dec'),
            list_forced_decays = cms.vstring('MyJ/psi'),
        ),
        parameterSets = cms.vstring('EvtGen')
    ),                             
    PythiaParameters = cms.PSet(
	pythiaUESettingsBlock,
        processParameters = cms.vstring('MSEL=61          ! Quarkonia', 
            'MDME(858,1) = 0  ! 0.060200    e-    e+', 
            'MDME(859,1) = 1  ! 0.060100    mu-  mu+', 
            'MDME(860,1) = 0  ! 0.879700    rndmflav        rndmflavbar', 
            'MSTP(142)=2      ! turns on the PYEVWT Pt re-weighting routine', 
            'PARJ(13)=0.750   ! probability that a c or b meson has S=1', 
            'PARJ(14)=0.162   ! probability that a meson with S=0 is produced with L=1, J=1', 
            'PARJ(15)=0.018   ! probability that a meson with S=1 is produced with L=1, J=0', 
            'PARJ(16)=0.054   ! probability that a meson with S=1 is produced with L=1, J=1', 
            'MSTP(145)=0      ! choice of polarization', 
            'MSTP(146)=0      ! choice of polarization frame ONLY when mstp(145)=1', 
            'MSTP(147)=0      ! particular helicity or density matrix component when mstp(145)=1', 
            'MSTP(148)=1      ! possibility to allow for final-state shower evolution, extreme case !', 
            'MSTP(149)=1      ! if mstp(148)=1, it determines the kinematics of the QQ~3S1(8)->QQ~3S1(8)+g branching', 
            'PARP(141)=1.16   ! New values for COM matrix elements', 
            'PARP(142)=0.0119 ! New values for COM matrix elements', 
            'PARP(143)=0.01   ! New values for COM matrix elements', 
            'PARP(144)=0.01   ! New values for COM matrix elements', 
            'PARP(145)=0.05   ! New values for COM matrix elements', 
            'PARP(146)=9.28   ! New values for COM matrix elements', 
            'PARP(147)=0.15   ! New values for COM matrix elements', 
            'PARP(148)=0.02   ! New values for COM matrix elements', 
            'PARP(149)=0.02   ! New values for COM matrix elements', 
            'PARP(150)=0.085  ! New values for COM matrix elements', 
            'BRAT(861)=0.202  ! chi_2c->J/psi gamma', 
            'BRAT(862)=0.798  ! chi_2c->rndmflav rndmflavbar', 
            'BRAT(1501)=0.013 ! chi_0c->J/psi gamma', 
            'BRAT(1502)=0.987 ! chi_0c->rndmflav rndmflavbar', 
            'BRAT(1555)=0.356 ! chi_1c->J/psi gamma', 
            'BRAT(1556)=0.644 ! chi_1c->rndmflav rndmflavbar'),
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
    ParticleID = cms.untracked.int32(443)
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

