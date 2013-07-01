import FWCore.ParameterSet.Config as cms
def customiseHiGenParticles(process):

    process.hiGenParticles.abortOnUnknownPDGCode = cms.untracked.bool(False)

    return(process)
