import FWCore.ParameterSet.Config as cms

process = cms.Process("LHE")

process.load("IOMC.RandomEngine.IOMC_cff")
process.load("FWCore.MessageService.MessageLogger_cfi")
process.load('Configuration.EventContent.EventContent_cff')

process.source = cms.Source("EmptySource",
    firstLuminosityBlock = cms.untracked.uint32(23456),
    numberEventsInLuminosityBlock = cms.untracked.uint32(10)
)                            

process.maxEvents = cms.untracked.PSet( input = cms.untracked.int32(100000) )

#############
# LHE
#############
process.load("GeneratorInterface/LHEInterface/ExternalLHEProducer_cfi")
process.externalLHEProducer.nEvents = process.maxEvents.input.value()
#process.outputFile = cms.string("/tmp/ochando/events.lhe"),

process.externalLHEProducer.scriptName = cms.FileInPath("GeneratorInterface/LHEInterface/data/create_lhe_powheg.sh")
process.externalLHEProducer.args = cms.vstring('slc5_ia32_gcc434/powheg/V1.0/src',
                                               'powhegboxv1.1_Feb2012',
                                               'Z',
                                               'slc5_ia32_gcc434/powheg/V1.0/8TeV_Summer12/DYToMuMu_M-20_CT10_8TeV-powheg/v2/DYToMuMu_M-20_CT10_8TeV-powheg.input')


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

process.e = cms.EndPath(process.out)
