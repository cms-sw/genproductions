import FWCore.ParameterSet.Config as cms
def customiseGeant4(process):

    process.g4SimHits.StackingAction.RusRoEcalNeutron = cms.double(0.1)
    process.g4SimHits.StackingAction.RusRoEcalNeutronLimit = cms.double(10.)
    process.g4SimHits.StackingAction.RusRoHcalNeutron = cms.double(0.1)
    process.g4SimHits.StackingAction.RusRoHcalNeutronLimit = cms.double(10.)

    return(process)
