import FWCore.ParameterSet.Config as cms

externalLHEProducer = cms.EDProducer('ExternalLHEProducer',
                                     scriptName = cms.FileInPath("GeneratorInterface/LHEInterface/data/run_madgraph_gridpack.sh"),
                                     outputFile = cms.string("DYJetsToLL_M-50_13TeV-madgraph_final.lhe"),
                                     numberOfParameters = cms.uint32(10),
                                     args = cms.vstring('slc5_ia32_gcc434/madgraph/V5_1.4.8/13TeV/DYJetsToLL_M-50_13TeV-madgraph/v1', #gridpack path
                                                        'DYJetsToLL_M-50_13TeV-madgraph', #gridpack name, without _gridpack.tar.gz
                                                        'false', #DECAY
                                                        'true', #REPLACE
                                                        'zjets', #process
                                                        '5',     #maxjetflavour
                                                        '20',    #qcut
                                                        'true',  #minmaxjet
                                                        '0',     #minjet
                                                        '4',     #maxjet
                                                        ),
                                     nEvents = cms.uint32(50000)
                                     )

