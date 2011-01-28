import FWCore.ParameterSet.Config as cms

from SimGeneral.MixingModule.mix_E7TeV_ProbDist_2010Data_BX156_cfi import *

def customise(process):

    process.mix.minBunch = cms.int32(-3)
    process.mix.maxBunch = cms.int32(2)
    process.mix.bunchspace = cms.int32(50)
    process.mix.input.nbPileupEvents.probFunctionVariable = cms.vint32(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24)
    process.mix.input.nbPileupEvents.probValue = cms.vdouble(0.04593,0.04593,0.04593,0.04593,0.04593,0.04593,0.04593,0.04593,0.04593,0.04593,0.04593,0.04593,0.04593,0.04593,0.04593,0.04593,0.04593,0.04593,0.04593,0.04593,0.04593,0.01965,0.00953,0.00440,0.00196)
    
    return(process)

