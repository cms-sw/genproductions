import FWCore.ParameterSet.Config as cms

externalLHEProducer = cms.EDProducer('ExternalLHEProducer',
    scriptName = cms.FileInPath("GeneratorInterface/LHEInterface/data/run_madgraph_gridpack.sh"),
    numberOfParameters = cms.uint32(10),                                     
    outputFile = cms.string("W4JetsToLNu_scaledown_8TeV-madgraph_final.lhe"),
    args = cms.vstring('slc5_ia32_gcc434/madgraph/V5_1.3.30/8TeV_Summer12/W4JetsToLNu_scaledown_8TeV-madgraph/v3',
    'W4JetsToLNu_scaledown_8TeV-madgraph','false','true','wjets','5','20','true','4','4'),
    nEvents = cms.uint32(10000)
)


