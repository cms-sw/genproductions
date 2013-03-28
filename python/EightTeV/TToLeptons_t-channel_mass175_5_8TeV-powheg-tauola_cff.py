import FWCore.ParameterSet.Config as cms

externalLHEProducer = cms.EDProducer('ExternalLHEProducer',
                                     scriptName = cms.FileInPath("GeneratorInterface/LHEInterface/data/create_lhe_powheg.sh"),
                                     outputFile = cms.string("events_final.lhe"),
                                     numberOfParameters = cms.uint32(4),
                                     args = cms.vstring('slc5_ia32_gcc434/powheg/V1.0/src',
                                     'powhegboxv1.0_Feb2013',
                                     'ST_tch',
                                     '/slc5_ia32_gcc434/powheg/V1.0/8TeV_Summer12/TToLeptons_t-channel_mass175_5_8TeV-powheg/v1/TToLeptons_t-channel_mass175_5_8TeV-powheg.input',
                                     ),
    nEvents = cms.uint32(3000000)
                                            
                                             )

