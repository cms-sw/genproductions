
import FWCore.ParameterSet.Config as cms

process = cms.Process("LHE")

process.load("IOMC.RandomEngine.IOMC_cff")
process.load("FWCore.MessageService.MessageLogger_cfi")
process.load('Configuration.EventContent.EventContent_cff')

process.source = cms.Source("EmptySource",
    firstLuminosityBlock = cms.untracked.uint32(23456),
    numberEventsInLuminosityBlock = cms.untracked.uint32(10)
)                            

#this is actually is used also as seed... not too good..
process.maxEvents = cms.untracked.PSet( input = cms.untracked.int32(100000) )

process.externalLHEProducer = cms.EDProducer('ExternalLHEProducer',
                                             scriptName = cms.FileInPath("GeneratorInterface/LHEInterface/data/run_madgraph_tarball.sh"),
                                             outputFile = cms.string("ZbbJetsToNuNu_Massive_8TeV-madgraph_unweighted_events_final.lhe"),
                                             args = cms.vstring( '/slc5_ia32_gcc434/madgraph/V5_1.3.30/8TeV_Summer12/ZbbJetsToNuNu_Massive_8TeV-madgraph/v1/', #gridpack path
                                                                 'ZbbJetsToNuNu_Massive_8TeV-madgraph', #gridpack name, without _tarball.tar.gz
                                                                 'false', #DECAY
                                                                 'false', #REPLACE
                                                                 'zjets', #process
                                                                 '4', #maxjetflavour
                                                                 '28', #qcut
                                                                 'true', #minmaxjet
                                                                 '0', #minjet
                                                                 '2', #maxjet
                                                                 '100000' #nevents
                                                                 ),
                                             nEvents = cms.uint32(100000)                       
                                             )


process.externalLHEProducer.nEvents = process.maxEvents.input.value()

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
