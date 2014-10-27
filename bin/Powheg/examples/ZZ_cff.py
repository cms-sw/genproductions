import FWCore.ParameterSet.Config as cms

externalLHEProducer = cms.EDProducer('ExternalLHEProducer',
                                     scriptName = cms.FileInPath("GeneratorInterface/LHEInterface/data/run_generic_tarball.sh"),
                                     outputFile = cms.string('eiko_ZZ_final.lhe'),
                                     numberOfParameters = cms.uint32(2),
                                     args = cms.vstring('slc6_amd64_gcc481/powheg/tarball/',
                                                        'eiko_ZZ'
                                                        ),
                                     nEvents = cms.uint32(100)
                                     )
