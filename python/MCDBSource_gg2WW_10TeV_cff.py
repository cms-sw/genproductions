import FWCore.ParameterSet.Config as cms

process = cms.Process('LHE')

process.source = cms.Source("MCDBSource",
    articleID = cms.uint32(219),
    supportedProtocols = cms.vstring('rfio')
)

process.maxEvents = cms.untracked.PSet(
    input = cms.untracked.int32(-1)
)

process.configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    annotation = cms.untracked.string('GG2WW sample at 10 TeV'),
    name = cms.untracked.string('$Source: /local/projects/CMSSW/rep/CMSSW/Configuration/GenProduction/python/MCDBSource_gg2WW_10TeV_cff.py,v $')
)

process.load('FWCore/MessageService/MessageLogger_cfi')


process.options = cms.untracked.PSet(
    Rethrow = cms.untracked.vstring('ProductNotFound')
)

process.output = cms.OutputModule("PoolOutputModule",
    fileName = cms.untracked.string('MCDBSource_gg2WW_10TeV_cff_py_LHE.root'),
    dataset = cms.untracked.PSet(
        dataTier = cms.untracked.string('USER'),
        filterName = cms.untracked.string('')
    )
)

process.out_step = cms.EndPath(process.output)
process.schedule = cms.Schedule(process.out_step)
