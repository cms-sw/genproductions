import FWCore.ParameterSet.Config as cms
from Configuration.Generator.PyquenTuneZ2Settings_cff import *

hiSignal = cms.EDFilter("PyquenGeneratorFilter",
                        collisionParameters,
                        qgpParameters,
                        pyquenParameters,

                        doQuench = cms.bool(False),
                        doIsospin = cms.bool(True),
                        protonSide = cms.untracked.int32(1),
                        
                        bFixed = cms.double(0.0), ## fixed impact param (fm); valid only if cflag_=0
                        PythiaParameters = cms.PSet(pyquenPythiaDefaultBlock,
                                                    parameterSets = cms.vstring('pythiaUESettings',
                                                                                'ppJets',
                                                                                'kinematics'),
                                                    kinematics = cms.vstring ("CKIN(3)=300",  #min pthat
                                                                              "CKIN(4)=9999" #max pthat
                                                                              )
                                                    ),
                        cFlag = cms.int32(0), ## centrality flag
                        bMin = cms.double(0.0), ## min impact param (fm); valid only if cflag_!=0
                        bMax = cms.double(0.0) ## max impact param (fm); valid only if cflag_!=0
                        )

hiSignal.embeddingMode = True

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.5 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/HI/Pyquen_Dijet300_pN_Unquenched_TuneZ2_5p02TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYQUEN UNquenched dijets (pt-hat > 300 GeV) at sqrt(s) = 5.02TeV')
    )

ProductionFilterSequence = cms.Sequence(hiSignal)

generator = hiSignal.clone(
    embeddingMode = False
    )
