import FWCore.ParameterSet.Config as cms

def customise(process):

        process.VtxSmeared.Phi = cms.double(0.0)
        process.VtxSmeared.BetaStar = cms.double(200.0)
        process.VtxSmeared.Emittance = cms.double(0.536e-07)
        process.VtxSmeared.Alpha = cms.double(0.0)
        process.VtxSmeared.SigmaZ = cms.double(2.87)
        process.VtxSmeared.TimeOffset = cms.double(0.0)
        process.VtxSmeared.X0 = cms.double(0.2434)
        process.VtxSmeared.Y0 = cms.double(0.3834)
        process.VtxSmeared.Z0 = cms.double(0.7552)         

	process.genParticles.abortOnUnknownPDGCode = False
	
        return(process)                                     
