import FWCore.ParameterSet.Config as cms

def customise(process):

        process.VtxSmeared.Phi = cms.double(0.0)
        process.VtxSmeared.BetaStar = cms.double(1100.0)
        process.VtxSmeared.Emittance = cms.double(1.0e-07)
        process.VtxSmeared.Alpha = cms.double(0.0)
        process.VtxSmeared.SigmaZ = cms.double(4.5)
        process.VtxSmeared.TimeOffset = cms.double(0.0)
        process.VtxSmeared.X0 = cms.double(0.2542)
        process.VtxSmeared.Y0 = cms.double(0.4082)
        process.VtxSmeared.Z0 = cms.double(-0.6569)         
                                                                
        return(process)                                     
