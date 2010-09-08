import FWCore.ParameterSet.Config as cms

def customise(process):

    # MODIFIED MIXING-MODULE CONFIGURATION FOR CLOSURE TEST
    process.mix.input.nbPileupEvents.averageNumber = cms.double(1.6)
    process.mix.bunchspace = cms.int32(150)
                
    return(process)
