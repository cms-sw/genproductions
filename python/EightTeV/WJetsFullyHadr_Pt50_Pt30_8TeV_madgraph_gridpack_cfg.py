
import FWCore.ParameterSet.Config as cms

externalLHEProducer = cms.EDProducer('ExternalLHEProducer',
    scriptName = cms.FileInPath("GeneratorInterface/LHEInterface/data/run_madgraph_gridpack.sh"),
    numberOfParameters = cms.uint32(10),
    outputFile = cms.string("WJetsFullyHard_8TeV-madgraph_final.lhe"),
    args = cms.vstring('slc5_ia32_gcc434/madgraph/V5_1.4.8/8TeV_Summer12/WJetsFullyHard_8TeV-madgraph/v1','WJetsFullyHard_8TeV-madgraph','false','false','wjets','5','20','false','0','2'),
    nEvents = cms.uint32(50000)
)
