import FWCore.ParameterSet.Config as cms



generator = cms.EDProducer('ExternalLHEProducer',
    scriptName = cms.FileInPath("GeneratorInterface/LHEInterface/data/run_madgraph_gridpack.sh"),
    outputFile = cms.string("events_final.lhe"),
    args = cms.vstring('slc5_ia32_gcc434/madgraph/V5_1.3.27/8TeV_Summer12/QCD_HT-100To250_8TeV-madgraph/v1','QCD_HT-100To250_8TeV-madgraph','false','false','qcd','5','21','true','2','4'),
    nEvents = cms.uint32(100000)
)
