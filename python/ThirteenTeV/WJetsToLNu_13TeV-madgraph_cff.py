import FWCore.ParameterSet.Config as cms

externalLHEProducer = cms.EDProducer('ExternalLHEProducer',
                                     scriptName = cms.FileInPath("GeneratorInterface/LHEInterface/data/run_madgraph_gridpack.sh"),
                                     outputFile = cms.string("DYJetsToLL_M-50_13TeV-madgraph_final.lhe"),
                                     numberOfParameters = cms.uint32(10),
                                     args = cms.vstring('slc5_amd64_gcc472/13TeV/madgraph/V5_1.5.11/WJetsToLNu_13TeV/v1', #gridpack path
                                                        'WJetsToLNu_13TeV', #gridpack name, without _gridpack.tar.gz
                                                        'false', #DECAY
                                                        'false',  #REPLACE
                                                        'wjets', #process
                                                        '5',     #maxjetflavour
                                                        '20',    #qcut
                                                        'true',  #minmaxjet
                                                        '0',     #minjet
                                                        '4',     #maxjet
                                                        ),
                                     nEvents = cms.uint32(50000)
                                     )

