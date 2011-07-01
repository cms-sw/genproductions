import FWCore.ParameterSet.Config as cms

# Resets RECO parameters for HCal (MC) to use only two TimeSlices

def customise(process):

    process.hbheprereco.correctionPhaseNS = cms.double(8.0)
    process.hbheprereco.samplesToAdd = cms.int32(2)
    process.hbheprereco.tsFromDB = cms.bool(False)    
    
    return(process)

