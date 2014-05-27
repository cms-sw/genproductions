import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.HI.PyquenTuneZ2Settings_cff import *

hiSignal = cms.EDFilter("PyquenGeneratorFilter",
                        collisionParameters,
                        qgpParameters,
                        pyquenParameters,
                        doQuench = cms.bool(False),
                        bFixed = cms.double(0.0), ## fixed impact param (fm); valid only if cflag_=0
                        cFlag = cms.int32(0), ## centrality flag
                        bMin = cms.double(0.0), ## min impact param (fm); valid only if cflag_!=0
                        bMax = cms.double(0.0), ## max impact param (fm); valid only if cflag_!=0
                        
                        PythiaParameters = cms.PSet(
    customProcesses = cms.vstring('MSEL=0   ! User processes'),
    leadingOrderQCDPhotonChannel = cms.vstring(    # Leading order photons
    'MSUB(14)=1', # q+qbar->g+gamma
    'MSUB(18)=1', # q+qbar->gamma+gamma
    'MSUB(29)=1', # q+g->q+gamma
    'MSUB(114)=1', # g+g->gamma+gamma
    'MSUB(115)=1' # g+g->g+gamma
    ),
    parameterSets = cms.vstring('pythiaUESettings',
                                'customProcesses',
                                'leadingOrderQCDPhotonChannel',
                                'kinematics'),
    kinematics = cms.vstring('CKIN(3)=15',
                             'CKIN(4)=9999',
                             "CKIN(7)=-3.",  #min rapidity
                             "CKIN(8)=3."    #max rapidity
                             
                             )
    )
                        )


hiSignal.embeddingMode = True

ProductionFilterSequence = cms.Sequence(hiSignal)
