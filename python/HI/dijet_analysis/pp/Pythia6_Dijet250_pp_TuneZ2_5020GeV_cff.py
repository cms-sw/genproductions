import FWCore.ParameterSet.Config as cms

from Configuration.Generator.Pyquen2015Settings_cff import *

generator = cms.EDFilter("PyquenGeneratorFilter",
		        collisionParameters5020GeV,
                        qgpParameters,
                        pyquenParameters,

                        doQuench = cms.bool(False),
                        
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

generator.doIsospin = cms.bool(False)

configurationMetadata = cms.untracked.PSet(
    annotation = cms.untracked.string('PYTHIA (unquenched) dijets in NN (pt-hat > 250 GeV) at sqrt(s) = 2.76TeV')
    )

ProductionFilterSequence = cms.Sequence(generator)
