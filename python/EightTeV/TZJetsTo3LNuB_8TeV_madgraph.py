import FWCore.ParameterSet.Config as cms

externalLHEProducer = cms.EDProducer('ExternalLHEProducer',
       scriptName = cms.FileInPath("GeneratorInterface/LHEInterface/data/run_madgraph_gridpack.sh"),
       outputFile = cms.string("TZJetsTo3LNuB_8TeV_madgraph_final.lhe"),
       numberOfParameters = cms.uint32(10),
       args = cms.vstring('slc5_ia32_gcc434/madgraph/V5_1.4.8/8TeV_Summer12/TZJetsTo3LNuB_8TeV_madgraph/v1',
      'TZJetsTo3LNuB_8TeV_madgraph', 'false','true','zjets','5','15','false','1','2'),
       nEvents = cms.uint32(5000)
       )
