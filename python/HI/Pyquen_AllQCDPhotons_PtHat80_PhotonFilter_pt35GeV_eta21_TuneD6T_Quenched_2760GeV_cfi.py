import FWCore.ParameterSet.Config as cms
from CmsHI.genproductions.HI.PyquenTuneD6TSettings import *
hiSignal = cms.EDFilter("PyquenGeneratorFilter",
                        particlePt = cms.vdouble(35),
                        particleStatus = cms.vint32(1),
                        partons = cms.vint32(1, 2, 3, 4, 5,  # parton cut is not functioning 
                                             6, 21, 22),
                        partonPt = cms.vdouble(38.5, 38.5, 38.5, 38.5, 38.5,
                                               38.5, 38.5, 38.5),
                        filterType = cms.untracked.string('EcalGenEvtSelectorFrag'),
                        particles = cms.vint32(22),
                        partonStatus = cms.vint32(2, 2, 2, 2, 2,
                                                  2, 2, 1),
                        etaMax = cms.double(2.1),   # Photon eta cut
                        aBeamTarget = cms.double(208.0),
                        comEnergy = cms.double(2760.0),
                        qgpInitialTemperature = cms.double(1.0),
                        doCollisionalEnLoss = cms.bool(False),
                        qgpNumQuarkFlavor = cms.int32(0),
                        qgpProperTimeFormation = cms.double(0.1),
                        numQuarkFlavor = cms.int32(0),
                        hadronFreezoutTemperature = cms.double(0.14),
                        doRadiativeEnLoss = cms.bool(True),
                        backgroundLabel = cms.InputTag("generator"),
                        embeddingMode = cms.bool(True),
                        angularSpectrumSelector = cms.int32(0),
                        doIsospin = cms.bool(True),
                        doQuench = cms.bool(True),
                        cFlag = cms.int32(0),
                        bFixed = cms.double(0.0),
                        bMin = cms.double(0.0),
                        bMax = cms.double(0.0),
                        maxTries = cms.untracked.int32(5000),
                        PythiaParameters = cms.PSet(pythiaUESettingsBlock,
    customProcesses = cms.vstring('MSEL=0   ! User processes'),
                                                    kinematics = cms.vstring('CKIN(3)=80',
                                                                             'CKIN(4)=9999'
                                                                             ),
                                                    
                                                    allQCDPhotonChannel = cms.vstring(    'MSUB(11)=1', # q+q->q+q
                                                                                          'MSUB(12)=1', # q+qbar->q+qbar
                                                                                          'MSUB(13)=1', # q+qbar->g+g
                                                                                          'MSUB(28)=1', # q+g->q+g
                                                                                          'MSUB(53)=1', # g+g->q+qbar
                                                                                          'MSUB(68)=1', # g+g->g+g
                                                                                          # Leading order photons
                                                                                          'MSUB(14)=1', # q+qbar->g+gamma
                                                                                          'MSUB(18)=1', # q+qbar->gamma+gamma
                                                                                          'MSUB(29)=1', # q+g->q+gamma
                                                                                          'MSUB(114)=1', # g+g->gamma+gamma
                                                                                          'MSUB(115)=1' # g+g->g+gamma
                                                                                          ),
                                                    parameterSets = cms.vstring('pythiaUESettings',
                                                                                'customProcesses',
                                                                                'allQCDPhotonChannel',
                                                                                'kinematics')
                                                    )
                        )



