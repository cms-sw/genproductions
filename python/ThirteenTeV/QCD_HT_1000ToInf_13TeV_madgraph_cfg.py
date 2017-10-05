import FWCore.ParameterSet.Config as cms

externalLHEProducer = cms.EDProducer('ExternalLHEProducer',
    scriptName = cms.FileInPath("GeneratorInterface/LHEInterface/data/run_madgraph_gridpack.sh"),
    outputFile = cms.string("QCD_HT-1000ToInf_final.lhe"),
    numberOfParameters = cms.uint32(10),
    args = cms.vstring('slc5_amd64_gcc472/13TeV/madgraph/V5_1.5.11/QCD_HT-1000ToInf/v2',
    'QCD_HT-1000ToInf','false','false','qcd','5','75','true','2','4'),
    nEvents = cms.uint32(100000)
)
