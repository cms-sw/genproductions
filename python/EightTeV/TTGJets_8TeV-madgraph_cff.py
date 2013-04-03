import FWCore.ParameterSet.Config as cms

externalLHEProducer = cms.EDProducer('ExternalLHEProducer',
    scriptName = cms.FileInPath("GeneratorInterface/LHEInterface/data/run_madgraph_gridpack.sh"),
    outputFile = cms.string("TTGJets_8TeV-madgraph_final.lhe"),
    numberOfParameters = cms.uint32(10),
    args = cms.vstring('slc5_ia32_gcc434/madgraph/V5_1.3.30/8TeV_Summer12/TTGJets_8TeV-madgraph/v4',
    'TTGJets_8TeV-madgraph','true','false','ttbar','5','10','false','0','2'),
    nEvents = cms.uint32(50000)
)
