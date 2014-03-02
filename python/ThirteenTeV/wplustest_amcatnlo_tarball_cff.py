import FWCore.ParameterSet.Config as cms

externalLHEProducer = cms.EDProducer('ExternalLHEProducer',
                                     scriptName = cms.FileInPath("GeneratorInterface/LHEInterface/data/run_madgraph_gridpack.sh"),
                                     outputFile = cms.string("wplustset_final.lhe"),
                                     numberOfParameters = cms.uint32(2),
                                     args = cms.vstring('slc5_ia32_gcc434/madgraph/V5_1.4.8/13TeV/DYJetsToLL_M-50_13TeV-madgraph/v1', #gridpack path
                                                        'wplustset', #gridpack name, without _gridpack.tar.gz
                                                        ),
                                     nEvents = cms.uint32(1000)
                                     )

