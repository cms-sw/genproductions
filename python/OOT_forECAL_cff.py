import FWCore.ParameterSet.Config as cms
def customiseOOTforECAL(process):

    process.mix.minBunch = cms.int32(-12)
    process.mix.maxBunch = cms.int32(3)

    return(process)
