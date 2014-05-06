import FWCore.ParameterSet.Config as cms

externalLHEProducer = cms.EDProducer('ExternalLHEProducer',
                                     scriptName = cms.FileInPath("GeneratorInterface/LHEInterface/data/create_lhe_powheg_all.sh"),
                                     outputFile = cms.string("events_final.lhe"),
                                     numberOfParameters = cms.uint32(8),
                                     args = cms.vstring('slc5_ia32_gcc434/powheg/V1.0/src', #repository of powheg source code
                                                        'powhegboxv1.0_Oct2013', # version of powheg source code, not used for precompiled mode
                                                        'WZ',     # process name
                                                        'slc5_amd64_gcc472/13TeV/powheg/WminusZToENuMuMu_CT10_13TeV-powheg/v1/WminusZToENuMuMu_CT10_13TeV-powheg.input', # location of input data card  
                                                        'true',  #use a precompiled tar ball or not 
                                                        'false',  #if compiling during the run, create a tar ball or not
                                                        'slc5_amd64_gcc472/8TeV/powheg/precompiled_set_v1', # repository of precompiled tar ball
                                                        'powhegboxv1.0_Oct2013_precompiled_set_v1'  #name of precompiled tar ball without the .tar.gz mandatory extension
                                                        ),
                                     nEvents = cms.uint32(100000)
                                     )
