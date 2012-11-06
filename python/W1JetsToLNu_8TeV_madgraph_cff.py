
import FWCore.ParameterSet.Config as cms

externalLHEProducer = cms.EDProducer('ExternalLHEProducer',
    scriptName = cms.FileInPath("GeneratorInterface/LHEInterface/data/run_madgraph_gridpack.sh"),
    outputFile = cms.string("W1JetsToLNu_8TeV-madgraph_final.lhe"),
    args = cms.vstring('slc5_ia32_gcc434/madgraph/V5_1.3.30/8TeV_Summer12/W1JetsToLNu_8TeV-madgraph/v2','W1JetsToLNu_8TeV-madgraph','false','true','wjets','5','20','true','1','99'),
    nEvents = cms.uint32(10000)
)

