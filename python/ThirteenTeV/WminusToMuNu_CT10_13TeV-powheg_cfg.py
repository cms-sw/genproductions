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
process.maxEvents = cms.untracked.PSet( input = cms.untracked.int32(100000) )

#############
# LHE
#############
process.load("GeneratorInterface/LHEInterface/ExternalLHEProducer_cfi")
process.externalLHEProducer.nEvents = process.maxEvents.input.value()

#######################################################################################################
### The following lines are important for powheg LHE production, 8 arguments must be specified
process.externalLHEProducer.scriptName = cms.FileInPath("GeneratorInterface/LHEInterface/data/create_lhe_powheg_all.sh")
process.externalLHEProducer.numberOfParameters = cms.uint32(8)
process.externalLHEProducer.outputFile = cms.string('events_final.lhe')
process.externalLHEProducer.args = cms.vstring('slc5_ia32_gcc434/powheg/V1.0/src', #repository of powheg source code
                                               'powhegboxv1.0_Oct2013', # version of powheg source code, not used for precompiled mode
                                               'W',     # process name
                                               'slc5_amd64_gcc472/13TeV/powheg/WminusToMuNu_CT10_13TeV-powheg/v1/WminusToMuNu_CT10_13TeV-powheg.input', # location of input data card  
                                               'true',  #use a precompiled tar ball or not 
                                               'false',  #if compiling during the run, create a tar ball or not
                                               'slc5_amd64_gcc472/8TeV/powheg/precompiled_set_v1', # repository of precompiled tar ball
                                               'powhegboxv1.0_Oct2013_precompiled_set_v1'  #name of precompiled tar ball without the .tar.gz mandatory extension
                                               )
################################################################################################

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
