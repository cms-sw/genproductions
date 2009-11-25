# imports as needed
import FWCore.ParameterSet.Config as cms

# since a source is still needed   
source = cms.Source('EmptySource')    

# the definition of the generator filter
from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter('Pythia6GeneratorFilter',   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    crossSection = cms.untracked.double(5790000),                     
    filterEfficiency = cms.untracked.double(0.013),   
    pythiaHepMCVerbosity = cms.untracked.bool(False),   
    comEnergy = cms.double(2360.0),   
    maxEventsToPrint = cms.untracked.int32(0),   
    PythiaParameters = cms.PSet(   
        pythiaUESettings = cms.vstring(   
            'MSTJ(11)=3     ! Choice of the fragmentation function',   
            'MSTJ(22)=2     ! Decay those unstable particles',   
            'PARJ(71)=10 .  ! for which ctau  10 mm',   
            'MSTP(2)=1      ! which order running alphaS',   
            'MSTP(33)=0     ! no K factors in hard cross sections',   
            'MSTP(51)=7     ! structure function chosen',   
            'MSTP(81)=1     ! multiple parton interactions 1 is Pythia default',   
            'MSTP(82)=4     ! Defines the multi-parton model',   
            'MSTU(21)=1     ! Check on possible errors during program execution',   
            'PARP(82)=1.9409   ! pt cutoff for multiparton interactions',   
            'PARP(89)=1960. ! sqrts for which PARP82 is set',   
            'PARP(83)=0.5   ! Multiple interactions: matter distrbn parameter',   
            'PARP(84)=0.4   ! Multiple interactions: matter distribution parameter',   
            'PARP(90)=0.16  ! Multiple interactions: rescaling power',   
            'PARP(67)=2.5    ! amount of initial-state radiation',     
            'PARP(85)=1.0  ! gluon prod. mechanism in MI',   
            'PARP(86)=1.0  ! gluon prod. mechanism in MI',   
            'PARP(62)=1.25   ! ',   
            'PARP(64)=0.2    ! ',   
            'MSTP(91)=1     !',   
            'PARP(91)=2.1   ! kt distribution',   
            'PARP(93)=15.0  ! '),   
        processParameters = cms.vstring(   
            'MSEL=0               ! user defined subprocess',   
	     'MSUB(86) = 0   ! g + g -> J/Psi + g  #old singlet flags',   
	     'MSUB(87) = 0   ! g + g -> chi0_c + g  #old singlet flags',   
            'MSUB(88) = 0   ! g + g -> chi1_c + g  #old singlet flags',   
	     'MSUB(89) = 0   ! g + g -> chi2_c + g  #old singlet flags',   
            'MSUB(104) = 0   ! g + g -> chi0_c  #old singlet flags',   
	     'MSUB(105) = 0   ! g + g -> chi2_c  #old singlet flags',   
            'MSUB(106) = 0   ! g + g -> J/Psi + gamma   #old singlet flags',   
            'MSUB(107) = 0   ! g + gamma -> J/Psi + g   #old singlet flags',   
          'MSUB(108) = 0   ! gamma + gamma -> J/Psi + gamma  #old singlet flags',   
          'MSUB(421) = 1    !g + g -> J/Psi + g (3S1(1)+g) ',   
          'MSUB(422) = 1    !g + g -> 3S1(8) + g ',   
          'MSUB(423) = 1    !g + g -> 1S0(8) + g ',   
          'MSUB(424) = 1    !g + g -> 3PJ(8) + g ',   
          'MSUB(425) = 1    !q + g -> q + 3S1(8) ',   
          'MSUB(426) = 1    !q + g -> q + 1S0(8) ',   
          'MSUB(427) = 1    !q + g -> q + 3PJ(8) ',   
          'MSUB(428) = 1    !q + qbar -> 3S1(8) + g ',   
          'MSUB(429) = 1    !q + qbar -> 1S0(8) + g ',   
          'MSUB(430) = 1    !q + qbar -> 3PJ(8) + g ',   
          'MSUB(431) = 1    !g + g -> 3P0(1) + g   // --> chi_c0 ',   
          'MSUB(432) = 1    !g + g -> 3P1(1) + g   // --> chi_c1 ',   
          'MSUB(433) = 1    !g + g -> 3P2(1) + g   // --> chi_c2 ',   
          'MSUB(434) = 1    !q + g -> 3P0(1) + q ',   
          'MSUB(435) = 1    !q + g -> 3P1(1) + q ',   
          'MSUB(436) = 1    !q + g -> 3P2(1) + q ',   
          'MSUB(437) = 1    !q + qbar -> 3P0(1) + g ',   
          'MSUB(438) = 1    !q + qbar -> 3P1(1) + g ',   
          'MSUB(439) = 1    !q + qbar -> 3P2(1) + g ',   
          'MDME(858,1) = 1  ! 0.060200    e-    e+',    
          'MDME(859,1) = 0  ! 0.060100    mu-  mu+',    
          'MDME(860,1) = 0  ! 0.879700    rndmflav        rndmflavbar',    
          'MDME(1037,1)=0   ! 0.015000    d dbar',    
          'MDME(1038,1)=0   ! 0.045000    u ubar',    
          'MDME(1039,1)=0   ! 0.015000    s sbar',    
          'MDME(1040,1)=0   ! 0.045000    c cbar',    
          'MDME(1041,1)=0   ! 0.774300    g g g',    
          'MDME(1042,1)=0   ! 0.029000    gamma g',   
          'MSTP(142)=2    ! turns on the PYEVWT Pt re-weighting routine',    
          'PARJ(13)=0.750 ! probability that a c or b meson has S=1',    
          'PARJ(14)=0.162 ! probability that a meson with S=0 is produced with L=1, J=1',    
          'PARJ(15)=0.018 ! probability that a meson with S=1 is produced with L=1, J=0',    
          'PARJ(16)=0.054 ! probability that a meson with S=1 is produced with L=1, J=1',    
          'MSTP(145)=0    !  choice of polarization',    
          'MSTP(146)=0    ! choice of polarization frame ONLY when mstp(145)=1',    
          'MSTP(147)=0    ! particular helicity or density matrix component when mstp(145)=1',    
          'MSTP(148)=1   ! possibility to allow for final-state shower evolution, extreme case        !',    
          'MSTP(149)=1   ! if mstp(148)=1, it determines the kinematics of the QQ~3S1(8)->QQ~3S1(8)+g branching',    
          'PARP(141)=1.16 ! New values for COM matrix elements',    
          'PARP(142)=0.0119 ! New values for COM matrix elements',    
          'PARP(143)=0.01 ! New values for COM matrix elements',    
          'PARP(144)=0.01 ! New values for COM matrix elements',    
          'PARP(145)=0.05 ! New values for COM matrix elements',    
          'PARP(146)=9.28 ! New values for COM matrix elements',     
          'PARP(147)=0.15 ! New values for COM matrix elements',    
          'PARP(148)=0.02 ! New values for COM matrix elements',    
          'PARP(149)=0.02 ! New values for COM matrix elements',    
          'PARP(150)=0.085 ! New values for COM matrix elements',   
          'BRAT(861)=0.202  ! chi_2c->J/psi gamma',   
          'BRAT(862)=0.798  ! chi_2c->rndmflav rndmflavbar',   
          'BRAT(1501)=0.013 ! chi_0c->J/psi gamma',   
          'BRAT(1502)=0.987 ! chi_0c->rndmflav rndmflavbar',   
          'BRAT(1555)=0.356 ! chi_1c->J/psi gamma',   
          'BRAT(1556)=0.644 ! chi_1c->rndmflav rndmflavbar'),   
       # This is a vector of ParameterSet names to be read, in this order   
      parameterSets = cms.vstring(   
          'pythiaUESettings',    
          'processParameters',   
          'CSAParameters'),	    
      CSAParameters = cms.vstring('CSAMODE = 6     ! cross-section reweighted quarkonia')   
  )   
)   

elelgenfilter = cms.EDFilter('MCParticlePairFilter',
    ParticleCharge = cms.untracked.int32(-1),
    ParticleID1 = cms.untracked.vint32(11),
    ParticleID2 = cms.untracked.vint32(11),
    Status = cms.untracked.vint32(1, 1),
    MinPt = cms.untracked.vdouble(2.0, 2.0),
    MaxEta = cms.untracked.vdouble(2.5, 2.5),
    MinEta = cms.untracked.vdouble(-2.5, -2.5)
)

jpsigenfilter = cms.EDFilter('PythiaFilter',
    Status = cms.untracked.int32(2),
   MaxEta = cms.untracked.double(2000),
    MinEta = cms.untracked.double(-2000),
    MinPt = cms.untracked.double(0.0),
    ParticleID = cms.untracked.int32(443)
)


# the definition of the Filter Sequence
ProductionFilterSequence = cms.Sequence(generator*jpsigenfilter*elelgenfilter)

