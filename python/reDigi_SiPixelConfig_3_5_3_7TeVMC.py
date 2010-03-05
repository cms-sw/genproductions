import FWCore.ParameterSet.Config as cms

def customise(process):

    process.load("SimTracker.SiPixelDigitizer.PixelDigi_cfi")
    process.simSiPixelDigis.ThresholdInElectrons_FPix = 3000.0
    process.simSiPixelDigis.ThresholdInElectrons_BPix = 3500.0
    process.simSiPixelDigis.ThresholdSmearing_FPix = 210.0
    process.simSiPixelDigis.ThresholdSmearing_BPix = 245.0


    return(process)
    
