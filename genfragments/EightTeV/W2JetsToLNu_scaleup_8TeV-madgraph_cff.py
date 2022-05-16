import FWCore.ParameterSet.Config as cms

externalLHEProducer = cms.EDProducer('ExternalLHEProducer',
    scriptName = cms.FileInPath("GeneratorInterface/LHEInterface/data/run_madgraph_gridpack.sh"),
    outputFile = cms.string("W2JetsToLNu_scaleup_8TeV-madgraph_final.lhe"),
    numberOfParameters = cms.uint32(10),
    args = cms.vstring('slc5_ia32_gcc434/madgraph/V5_1.3.30/8TeV_Summer12/W2JetsToLNu_scaleup_8TeV-madgraph/v1',
    'W2JetsToLNu_scaleup_8TeV-madgraph','false','true','wjets','5','20','true','2','99'),
    nEvents = cms.uint32(100000)
)
