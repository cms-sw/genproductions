import FWCore.ParameterSet.Config as cms

externalLHEProducer = cms.EDProducer('ExternalLHEProducer',
    scriptName = cms.FileInPath("GeneratorInterface/LHEInterface/data/run_madgraph_gridpack.sh"),
    outputFile = cms.string("events.lhe"),
    numberOfParameters = cms.uint32(10),
    args = cms.vstring('slc5_amd64_gcc472/13TeV/madgraph/V5_1.5.11/QCD_HT-500To1000/v1',
    'QCD_HT-500To1000','false','false','qcd','5','54','true','2','4'),
    nEvents = cms.uint32(100000)
)
