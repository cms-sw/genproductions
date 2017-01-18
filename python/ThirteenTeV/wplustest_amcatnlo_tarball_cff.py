import FWCore.ParameterSet.Config as cms

externalLHEProducer = cms.EDProducer('ExternalLHEProducer',
                                     scriptName = cms.FileInPath("GeneratorInterface/LHEInterface/data/run_generic_tarball.sh"),
                                     outputFile = cms.string("wplustest_final.lhe"),
                                     numberOfParameters = cms.uint32(2),
                                     args = cms.vstring('slc6_amd64_gcc472/madgraph/V5_2.1.1/13TeV/wplustest/v1', #gridpack path
                                                        'wplustest', #gridpack name, without _gridpack.tar.gz
                                                        ),
                                     nEvents = cms.uint32(1000)
                                     )

