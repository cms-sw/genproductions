import FWCore.ParameterSet.Config as cms

externalLHEProducer = cms.EDProducer('ExternalLHEProducer',
    scriptName = cms.FileInPath("GeneratorInterface/LHEInterface/data/run_madgraph_gridpack.sh"),
    outputFile = cms.string("GJets_HT-100To200_8TeV-madgraph_final.lhe"),
    numberOfParameters = cms.uint32(10),
    args = cms.vstring('slc5_ia32_gcc434/madgraph/V5_1.3.30/8TeV_Summer12/GJets_HT-100To200_8TeV-madgraph/v3',
                       'GJets_HT-100To200_8TeV-madgraph',
                       'false','false','qcd','5','15','true','1','4'),
    nEvents = cms.uint32(30000)
)


