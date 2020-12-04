import FWCore.ParameterSet.Config as cms

from Configuration.Generator.PyquenDefaultSettings_cff import *
from GeneratorInterface.HiGenCommon.jpsiMuMuTrigSettings_cff import *


hiSignal = cms.EDFilter("PyquenGeneratorFilter",
                        jpsiMuMuTrigCommon,
                        collisionParameters2760GeV,
                        qgpParameters,
                        pyquenParameters,
                        doQuench = cms.bool(False),
                        bFixed = cms.double(0.0), ## fixed impact param (fm); valid only if cflag_=0
                        cFlag = cms.int32(0), ## centrality flag
                        bMin = cms.double(0.0), ## min impact param (fm); valid only if cflag_!=0
                        bMax = cms.double(0.0), ## max impact param (fm); valid only if cflag_!=0
                        ExternalDecays = cms.PSet(EvtGen = cms.untracked.PSet(use_internal_pythia = cms.untracked.bool(False),
                                                                              operates_on_particles = cms.vint32( 100443 ),
                                                                              use_default_decay = cms.untracked.bool(False),
                                                                              decay_table = cms.FileInPath('GeneratorInterface/ExternalDecays/data/DECAY_NOLONGLIFE.DEC'),
                                                                              particle_property_file = cms.FileInPath('GeneratorInterface/ExternalDecays/data/evt.pdl'),
                                                                              user_decay_file = cms.FileInPath('GeneratorInterface/ExternalDecays/data/Onia_mumu.dec'),
                                                                              list_forced_decays = cms.vstring('Mypsi(2S)'),
                                                                              ),
                                                  parameterSets = cms.vstring('EvtGen')
                                                  ),
                        PythiaParameters = cms.PSet(pyquenPythiaDefaultBlock,
                                                    parameterSets = cms.vstring('pythiaUESettings',
                                                                                'pythiaNRQCD',
                                                                                'CSAParameters',
                                                                                'kinematics'
                                                                                ),
                                                    kinematics = cms.vstring ("CKIN(7)=-3.",  #min rapidity
                                                                              "CKIN(8)=3."    #max rapidity
                                                                              ),
                                                    pythiaNRQCD = cms.vstring('MSEL=61          ! Quarkonia', 
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
                                                    CSAParameters = cms.vstring('CSAMODE=6     ! cross-section reweighted quarkonia'),
                                                    )
                        )

hiSignal.embeddingMode   = True
hiSignal.hadrons         = cms.vint32(100443)
hiSignal.hadronPtMax     = cms.vdouble(12.)
hiSignal.hadronPtMin     = cms.vdouble(9.)
ProductionFilterSequence = cms.Sequence(hiSignal)
