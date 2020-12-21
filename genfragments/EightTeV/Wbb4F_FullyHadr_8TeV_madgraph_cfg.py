
import FWCore.ParameterSet.Config as cms

externalLHEProducer = cms.EDProducer('ExternalLHEProducer',
    scriptName = cms.FileInPath("GeneratorInterface/LHEInterface/data/run_madgraph_gridpack.sh"),
    numberOfParameters = cms.uint32(10),
    outputFile = cms.string("Wbb4F_FullyHadr_8TeV_madgraph_final.lhe"),
    args = cms.vstring('slc5_ia32_gcc434/madgraph/V5_1.4.8/8TeV_Summer12/Wbb4F_FullyHadr_8TeV_madgraph/v3','Wbb4F_FullyHadr_8TeV_madgraph','false','false','qcd','4','6','false','0','2'),
    nEvents = cms.uint32(50000)
)
