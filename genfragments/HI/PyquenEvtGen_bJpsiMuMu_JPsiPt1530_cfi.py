import FWCore.ParameterSet.Config as cms

from Configuration.Generator.PyquenDefaultSettings_cff import *
from GeneratorInterface.HiGenCommon.bJpsiMuMuTrigSettings_cff import *

hiSignal = cms.EDFilter("PyquenGeneratorFilter",
                        bJpsiMuMuTrigPt1530,
                        collisionParameters2760GeV,
                        qgpParameters,
                        pyquenParameters,
                        doQuench = cms.bool(False),
                        bFixed = cms.double(0.0), ## fixed impact param (fm); valid only if cflag_=0
                        cFlag = cms.int32(0), ## centrality flag
                        bMin = cms.double(0.0), ## min impact param (fm); valid only if cflag_!=0
                        bMax = cms.double(0.0), ## max impact param (fm); valid only if cflag_!=0
                        ExternalDecays        = cms.PSet(EvtGen = cms.untracked.PSet(use_default_decay      = cms.untracked.bool(False),
                                                                                     decay_table            = cms.FileInPath('GeneratorInterface/ExternalDecays/data/DECAY_NOLONGLIFE.DEC'),
                                                                                     particle_property_file = cms.FileInPath('GeneratorInterface/ExternalDecays/data/evt.pdl'),
                                                                                     user_decay_file        = cms.FileInPath('GeneratorInterface/ExternalDecays/data/incl_BtoJpsi_mumu.dec'),
                                                                                     list_forced_decays     = cms.vstring('MyB0', 
                                                                                                                          'Myanti-B0',
                                                                                                                          'MyB+',
                                                                                                                          'MyB-',
                                                                                                                          'MyB_s0', 
                                                                                                                          'Myanti-B_s0'),
                                                                                     operates_on_particles  = cms.vint32( 0 ) # 0 (zero) means default list (hardcoded)
                                                                                     ),
                                                         parameterSets = cms.vstring('EvtGen')
                                                         ),
                        PythiaParameters = cms.PSet(pyquenPythiaDefaultBlock,
                                                    parameterSets = cms.vstring('pythiaUESettings',
                                                                                'pythiaBBbar',
                                                                                'kinematics'
                                                                                ),
                                                    kinematics = cms.vstring ("CKIN(7)=-3.",  #min rapidity
                                                                              "CKIN(8)=3."    #max rapidity
                                                                              ),
                                                    pythiaBBbar = cms.vstring ("MSEL=1", # dijets
                                                                               ),
                                                    
                                                    )
                       
                        )

hiSignal.embeddingMode = True

ProductionFilterSequence = cms.Sequence(hiSignal)
