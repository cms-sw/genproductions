import FWCore.ParameterSet.Config as cms

# Resets RECO parameters for HCal (MC) to use only two TimeSlices

def customise(process):

    process.simHcalDigis.HBregion = cms.vint32(3,6)
    process.simHcalDigis.HEregion = cms.vint32(3,6)

    return(process)

