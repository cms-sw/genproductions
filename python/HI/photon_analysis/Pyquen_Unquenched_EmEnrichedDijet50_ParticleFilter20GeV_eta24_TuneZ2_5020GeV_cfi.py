import FWCore.ParameterSet.Config as cms

from Configuration.Generator.PyquenDefaultSettings_cff import *
from Configuration.Generator.PythiaUEZ2Settings_cfi import *

hiSignal = cms.EDFilter("PyquenGeneratorFilter",
                        qgpParameters,
                        pyquenParameters,
                        aBeamTarget = cms.double(208.0),
                        comEnergy = cms.double(5020.0),
                        doQuench = cms.bool(False),
                        cFlag = cms.int32(0), ## centrality flag
                        bMin = cms.double(0.0), ## min impact param (fm); valid only if cflag_!=0
                        bMax = cms.double(0.0), ## max impact param (fm); valid only if cflag_!=0
                        bFixed = cms.double(0.0), ## fixed impact param (fm); valid only if cflag_=0


                        filterType = cms.untracked.string('EcalGenEvtSelector'),
                        partons = cms.vint32(1, 2, 3, 4, 5, 6, #quarks
                                             21, 22), #gluon, photon
                        partonPt = cms.vdouble(0, 0, 0, 0, 0, 0,
                                               0, 0),
                        partonStatus = cms.vint32(2, 2, 2, 2, 2, 2,
                                                  2, 1),
                        particles = cms.vint32(221, #eta
                                               331, #eta'
                                               223, #omega
                                               111), #pi0
                        particlePt = cms.vdouble(20, 20, 20, 20),
                        particleStatus = cms.vint32(2, #eta
                                                    2, #eta'
                                                    2, #omega
                                                    2), #pi0
                        etaMax = cms.double(2.4),   # Photon eta cut
                        maxTries = cms.untracked.int32(5000),

                        PythiaParameters = cms.PSet(
                            pythiaUESettingsBlock,
                            ppJets = cms.vstring('MSEL=1   ! QCD hight pT processes'),
                            parameterSets = cms.vstring('pythiaUESettings',
                                                        'ppJets',
                                                        'kinematics'),

                            kinematics = cms.vstring('CKIN(3)=50',
                                                     'CKIN(4)=9999'
                                                 )
                        )
)


hiSignal.embeddingMode = False
ProductionFilterSequence = cms.Sequence(hiSignal)
