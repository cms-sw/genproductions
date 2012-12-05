
import FWCore.ParameterSet.Config as cms

externalLHEProducer = cms.EDProducer('ExternalLHEProducer',
    scriptName = cms.FileInPath("GeneratorInterface/LHEInterface/data/run_madgraph_gridpack.sh"),
    numberOfParameters = cms.uint32(10),
    outputFile = cms.string("diPhoton_M0_7TeV_madgraph_final.lhe"),
    args = cms.vstring('slc5_ia32_gcc434/madgraph/V5_1.4.48/8TeV_Summer12/diPhoton_M0_7TeV_madgraph/v1','diPhoton_M0_7TeV_madgraph','false','false','zjets','5','7','false','0','2'),
    nEvents = cms.uint32(100000)
)
