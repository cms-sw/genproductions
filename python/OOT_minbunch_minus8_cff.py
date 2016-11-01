import FWCore.ParameterSet.Config as cms
def customiseOOTnbunchesminus8(process):

    process.mix.minBunch = cms.int32(-8)

    return(process)