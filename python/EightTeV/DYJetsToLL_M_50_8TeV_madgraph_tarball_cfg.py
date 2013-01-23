import FWCore.ParameterSet.Config as cms

process.externalLHEProducer = cms.EDProducer('ExternalLHEProducer',
    scriptName = cms.FileInPath("GeneratorInterface/LHEInterface/data/run_madgraph_tarball.sh"),
    outputFile = cms.string("DYJetsToLL_M-50_8TeV-madgraph_unweighted_events_final.lhe"),
    args = cms.vstring('slc5_ia32_gcc434/madgraph/V5_1.3.30/8TeV_Summer12/DYJetsToLL_M-50_8TeV-madgraph/v2',
    'DYJetsToLL_M-50_8TeV-madgraph','false','true','zjets','5','20','true','0','4'),
    nEvents = cms.uint32(process.maxEvents.input.value())
)

