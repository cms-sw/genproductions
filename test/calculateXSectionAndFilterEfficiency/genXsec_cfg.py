import FWCore.ParameterSet.Config as cms
from FWCore.ParameterSet.VarParsing import VarParsing
options = VarParsing ('analysis')
options.parseArguments()
process = cms.Process('XSec')

process.maxEvents = cms.untracked.PSet(
    input = cms.untracked.int32(options.maxEvents)
)

process.load('FWCore.MessageService.MessageLogger_cfi')
process.MessageLogger.cerr.FwkReport.reportEvery = 100000

secFiles = cms.untracked.vstring() 
process.source = cms.Source ("PoolSource",
    fileNames = cms.untracked.vstring(options.inputFiles), 
    secondaryFileNames = secFiles)
process.xsec = cms.EDAnalyzer("GenXSecAnalyzer")

process.ana = cms.Path(process.xsec)
process.schedule = cms.Schedule(process.ana)
