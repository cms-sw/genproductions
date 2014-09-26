import FWCore.ParameterSet.Config as cms

process = cms.Process("LHE")

process.load("IOMC.RandomEngine.IOMC_cff")
process.load("FWCore.MessageService.MessageLogger_cfi")
process.load('Configuration.EventContent.EventContent_cff')

process.source = cms.Source("EmptySource",
    firstLuminosityBlock = cms.untracked.uint32(23456),
    numberEventsInLuminosityBlock = cms.untracked.uint32(10)
)                            
process.load("GeneratorInterface.LHEInterface.ExternalLHEAsciiDumper_cfi")
#the following two parameters need to be changed on a job by job basis
process.externalLHEAsciiDumper.lheFileName = cms.string('output.lhe')
process.RandomNumberGeneratorService.externalLHEProducer.initialSeed = 1111111 
process.maxEvents = cms.untracked.PSet( input = cms.untracked.int32(100) )

#############
# LHE
#############
process.load("GeneratorInterface/LHEInterface/ExternalLHEProducer_cfi")
process.externalLHEProducer.nEvents = process.maxEvents.input.value()

process.externalLHEProducer.scriptName = cms.FileInPath("GeneratorInterface/LHEInterface/data/run_generic_tarball.sh")

process.externalLHEProducer.numberOfParameters = cms.uint32(2)
process.externalLHEProducer.outputFile = cms.string('events_final.lhe')

process.externalLHEProducer.args = cms.vstring(
                                               'slc6_amd64_gcc481/powheg/tarball/',
                                               'eiko_ZZ'
                                               )



#############
# Output
#############
process.out = cms.OutputModule("PoolOutputModule",
    splitLevel = cms.untracked.int32(0),
    eventAutoFlushCompressedSize = cms.untracked.int32(5242880),
    outputCommands = process.LHEEventContent.outputCommands,
    fileName = cms.untracked.string('lheOutputFile.root'),
    dataset = cms.untracked.PSet(
        filterName = cms.untracked.string(''),
        dataTier = cms.untracked.string('LHE')
    )
)

process.p = cms.Path(process.externalLHEProducer)
process.e = cms.EndPath(process.externalLHEAsciiDumper)
