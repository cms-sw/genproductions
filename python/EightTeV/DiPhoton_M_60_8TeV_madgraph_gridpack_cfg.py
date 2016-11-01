
import FWCore.ParameterSet.Config as cms

process = cms.Process("ScriptExample")

process.load("IOMC.RandomEngine.IOMC_cff")
process.load("FWCore.MessageService.MessageLogger_cfi")
process.MessageLogger.categories=cms.untracked.vstring('FwkJob'
                                                           ,'FwkReport'
                                                           ,'FwkSummary'
                                                           ,'Root_NoDictionary'
                                                           ,'Generator'
                                                           ,'LHEInterface'
                                                           )
process.MessageLogger.cerr.INFO = cms.untracked.PSet(limit = cms.untracked.int32(-1))
process.MessageLogger.cerr.Generator = cms.untracked.PSet(limit = cms.untracked.int32(0))
process.MessageLogger.cerr.LHEInterface = cms.untracked.PSet(limit = cms.untracked.int32(0))
process.MessageLogger.cerr.FwkReport.reportEvery = cms.untracked.int32(10000)
process.load('Configuration.EventContent.EventContent_cff')
process.load("GeneratorInterface.LHEInterface.ExternalLHEAsciiDumper_cfi")
#the following two parameters need to be changed on a job by job basis
process.externalLHEAsciiDumper.lheFileName = cms.string('output.lhe') 
process.RandomNumberGeneratorService.externalLHEProducer.initialSeed = 1111111

process.source = cms.Source("EmptySource",
    firstLuminosityBlock = cms.untracked.uint32(23456),
    numberEventsInLuminosityBlock = cms.untracked.uint32(10)
)                            

process.maxEvents = cms.untracked.PSet( input = cms.untracked.int32(30000) )

process.externalLHEProducer = cms.EDProducer('ExternalLHEProducer',
    scriptName = cms.FileInPath("GeneratorInterface/LHEInterface/data/run_madgraph_gridpack.sh"),
    outputFile = cms.string("events_final.lhe"),
    args = cms.vstring('slc5_ia32_gcc434/madgraph/V5_1.3.30/8TeV_Summer12/DiPhoton_m60_8TeV_madgraph/v1', 'DiPhoton_m60_8TeV_madgraph', 'false','false','zjets','5','4','false','0','2'),
    nEvents = cms.uint32(process.maxEvents.input.value())
)


process.out = cms.OutputModule("PoolOutputModule",
    splitLevel = cms.untracked.int32(0),
    eventAutoFlushCompressedSize = cms.untracked.int32(5242880),
    outputCommands = process.LHEEventContent.outputCommands,
    fileName = cms.untracked.string('myOutputFile.root'),
    dataset = cms.untracked.PSet(
        filterName = cms.untracked.string(''),
        dataTier = cms.untracked.string('LHE')
    )
)

process.p = cms.Path(process.externalLHEProducer)

process.e = cms.EndPath(process.externalLHEAsciiDumper)
#+process.out)
