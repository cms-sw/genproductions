import FWCore.ParameterSet.Config as cms

externalLHEProducer = cms.EDProducer('ExternalLHEProducer',
    scriptName = cms.FileInPath("GeneratorInterface/LHEInterface/data/run_madgraph_gridpack.sh"),
    outputFile = cms.string("WZJetsTo3LNu_matchingdown_8TeV_madgraph_final.lhe"),
    numberOfParameters = cms.uint32(10),
    args = cms.vstring('slc5_ia32_gcc434/madgraph/V5_1.4.8/8TeV_Summer12/WZJetsTo3LNu_matchingdown_8TeV_madgraph/v1',
    'WZJetsTo3LNu_matchingdown_8TeV_madgraph','false','true','wjets','5','7','true','0','2'),
    nEvents = cms.uint32(10000)
)
