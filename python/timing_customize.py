import FWCore.ParameterSet.Config as cms

def customise(process):

        process.Timing = cms.Service("Timing")
        process.options = cms.untracked.PSet( wantSummary = cms.untracked.bool(True) )
        process.genParticles.abortOnUnknownPDGCode = False

        return(process)

