import FWCore.ParameterSet.Config as cms

from Configuration.Generator.PyquenTuneZ2Settings_cff import *

hiSignal = cms.EDFilter("PyquenGeneratorFilter",
                        collisionParameters,
                        qgpParameters,
                        pyquenParameters,
                        doQuench = cms.bool(True),
                        bFixed = cms.double(0.0), ## fixed impact param (fm); valid only if cflag_=0
                        PythiaParameters = cms.PSet(pyquenPythiaDefaultBlock,
                                                    parameterSets = cms.vstring('pythiaUESettings',
                                                                                'ppJets',
                                                                                'kinematics'),
                                                    kinematics = cms.vstring ("CKIN(3)=250",  #min pthat
                                                                              "CKIN(4)=9999" #max pthat
                                                                              )
                                                    ),
                        cFlag = cms.int32(0), ## centrality flag
                        bMin = cms.double(0.0), ## min impact param (fm); valid only if cflag_!=0
                        bMax = cms.double(0.0) ## max impact param (fm); valid only if cflag_!=0
                        )

hiSignal.embeddingMode = True

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /local/reps/CMSSW/CMSSW/Configuration/GenProduction/python/HI/Pyquen_DiJet_Pt250_TuneZ2_Quenched_2760GeV_cfi.py,v $'),
    annotation = cms.untracked.string('PYQUEN quenched dijets (pt-hat > 250 GeV) at sqrt(s) = 2.76TeV')
    )

ProductionFilterSequence = cms.Sequence(hiSignal)
