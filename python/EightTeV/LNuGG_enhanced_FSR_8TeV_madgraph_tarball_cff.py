import FWCore.ParameterSet.Config as cms

externalLHEProducer = cms.EDProducer('ExternalLHEProducer',
                                     scriptName = cms.FileInPath("GeneratorInterface/LHEInterface/data/run_madgraph_gridpack.sh"),
                                     outputFile = cms.string("ppWaa_enhanced_FSR_final.lhe"),
                                     numberOfParameters = cms.uint32(10),
                                     args = cms.vstring('slc5_ia32_gcc434/madgraph/V5_1.4.8/8TeV_Summer12/Waa_enhanced_FSR_madgraph/v2', #gridpack path
                                                        'ppWaa_enhanced_FSR', #gridpack name, without _gridpack.tar.gz
                                                        'false', #DECAY
                                                        'true', #REPLACE
                                                        'wjets', #process
                                                        '5', #maxjetflavour
                                                        '20', #qcut
                                                        'true', #minmaxjet
                                                        '0', #minjet
                                                        '1', #maxjet
                                                        ),
                                     nEvents = cms.uint32(50000)
                                     )

