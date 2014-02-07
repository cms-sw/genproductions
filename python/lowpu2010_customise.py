import FWCore.ParameterSet.Config as cms

def customise(process): # Using SL instead GFlash AND turn on castor and compensation
    process.g4SimHits.HCalSD.UseShowerLibrary = cms.bool(True)
    process.g4SimHits.HCalSD.UseParametrize = cms.bool(False)
    process.g4SimHits.HCalSD.UsePMTHits = cms.bool(False)
    process.g4SimHits.HCalSD.UseFibreBundleHits = cms.bool(False)
    process.g4SimHits.HFShower.UseShowerLibrary = cms.bool(True)
    process.g4SimHits.HFShower.UseHFGflash = cms.bool(False)
    process.g4SimHits.HFShower.ApplyFiducialCut = cms.bool(False)
    process.g4SimHits.Generator.MinEtaCut = cms.double(-7.0)
    process.g4SimHits.CastorSD.nonCompensationFactor = cms.double(0.77)

    return(process)



