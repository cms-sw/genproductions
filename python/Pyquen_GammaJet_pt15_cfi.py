import FWCore.ParameterSet.Config as cms

from Configuration.Generator.PyquenDefaultSettings_cff import *

hiSignal = cms.EDFilter("PyquenGeneratorFilter",
                         collisionParameters,
                         qgpParameters,
                         pyquenParameters,
                         doQuench = cms.bool(True),
                         bFixed = cms.double(0.0), ## fixed impact param (fm); valid only if cflag_=0
                         PythiaParameters = cms.PSet(pyquenPythiaDefaultBlock,
                                                     parameterSets = cms.vstring('pythiaUESettings',
                                                                                 'customProcesses',
                                                                                 'pythiaPromptPhotons',
                                                                                 'kinematics'),
                                                     kinematics = cms.vstring ("CKIN(3)=15",  #min pthat
                                                                               "CKIN(4)=9999", #max pthat
                                                                               "CKIN(7)=-3.",  #min rapidity
                                                                               "CKIN(8)=3."    #max rapidity
                                                                               )
                                                     
                                                     ),
                         cFlag = cms.int32(0), ## centrality flag
                         bMin = cms.double(0.0), ## min impact param (fm); valid only if cflag_!=0
                         bMax = cms.double(0.0) ## max impact param (fm); valid only if cflag_!=0
                         )

hiSignal.embeddingMode = True

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.4 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/Pyquen_GammaJet_pt15_cfi.py,v $'),
    annotation = cms.untracked.string('PYQUEN quenched gamma-jets (pt-hat > 15 GeV) at sqrt(s) = 2.76TeV')
    )

ProductionFilterSequence = cms.Sequence(hiSignal)
