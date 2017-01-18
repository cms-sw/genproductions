import FWCore.ParameterSet.Config as cms

def customise(process):
    process.load('GeneratorInterface.GenFilters.TotalKinematicsFilter_cfi')
    process.filtseq = cms.Sequence(process.totalKinematicsFilter)
    getattr(process,"generation_step")._seq = getattr(process,"generation_step")._seq * process.filtseq
    return(process)
