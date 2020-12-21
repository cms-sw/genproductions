import FWCore.ParameterSet.Config as cms

from Configuration.Generator.PyquenDefaultSettings_cff import *
from GeneratorInterface.HiGenCommon.upsilon1sMuMuTrigSettings_cff import *

hiSignal = cms.EDFilter("PyquenGeneratorFilter",
                        upsilon1sMuMuTrigPt1215,
                        collisionParameters2760GeV,
                        qgpParameters,
                        pyquenParameters,
                        doQuench = cms.bool(False),
                        bFixed = cms.double(0.0), ## fixed impact param (fm); valid only if cflag_=0
                        cFlag = cms.int32(0), ## centrality flag
                        bMin = cms.double(0.0), ## min impact param (fm); valid only if cflag_!=0
                        bMax = cms.double(0.0), ## max impact param (fm); valid only if cflag_!=0
                        ExternalDecays = cms.PSet(EvtGen = cms.untracked.PSet(operates_on_particles = cms.vint32( 0 ), # 0 (zero) means default list (hardcoded)
                                                                              # you can put here the list of particles (PDG IDs)
                                                                              # that you want decayed by EvtGen
                                                                              use_default_decay = cms.untracked.bool(False),
                                                                              decay_table = cms.FileInPath('GeneratorInterface/ExternalDecays/data/DECAY_NOLONGLIFE.DEC'),
                                                                              particle_property_file = cms.FileInPath('GeneratorInterface/ExternalDecays/data/evt.pdl'),
                                                                              user_decay_file = cms.FileInPath('GeneratorInterface/ExternalDecays/data/Onia_mumu.dec'),
                                                                              list_forced_decays = cms.vstring('MyUpsilon'),
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
                                                    pythiaNRQCD = cms.vstring('MSEL=62          ! Quarkonia NRQCD bb ', 
                                                                              'MDME(1034,1)=0   ! 0.025200    e- e+', 
                                                                              'MDME(1035,1)=1   ! 0.024800    mu- mu+', 
                                                                              'MDME(1036,1)=0   ! 0.026700    tau- tau+', 
                                                                              'MDME(1037,1)=0   ! 0.015000    d dbar', 
                                                                              'MDME(1038,1)=0   ! 0.045000    u ubar', 
                                                                              'MDME(1039,1)=0   ! 0.015000    s sbar', 
                                                                              'MDME(1040,1)=0   ! 0.045000    c cbar', 
                                                                              'MDME(1041,1)=0   ! 0.774300    g g g', 
                                                                              'MDME(1042,1)=0   ! 0.029000    gamma g', 
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
                                                                              'PARP(150)=0.085  ! New values for COM matrix elements'),
                                                    CSAParameters = cms.vstring('CSAMODE=6     ! cross-section reweighted quarkonia')
                                                    )
                        )

hiSignal.embeddingMode = True

ProductionFilterSequence = cms.Sequence(hiSignal)
