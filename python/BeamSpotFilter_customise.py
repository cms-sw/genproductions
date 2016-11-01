import FWCore.ParameterSet.Config as cms

def customiseBeamSpot(process):

    process.load('IOMC.EventVertexGenerators.GaussianZBeamSpotFilter_cfi')

    process.beamSpotFilter = cms.Sequence(process.simBeamSpotFilter)
    for path in process.paths:
        getattr(process,path)._seq = process.beamSpotFilter * getattr(process,path)._seq 

    for item in process.outputModules_().values():
        item.SelectEvents = cms.untracked.PSet(SelectEvents = cms.vstring('digitisation_step'))

    return(process)
