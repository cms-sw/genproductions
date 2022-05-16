import FWCore.ParameterSet.Config as cms

externalLHEProducer = cms.EDProducer('ExternalLHEProducer',
    scriptName = cms.FileInPath("GeneratorInterface/LHEInterface/data/run_madgraph_gridpack.sh"),
    outputFile = cms.string("Z_VBF_Lept_mqq_120_inf_8TeV_madgraph_final.lhe"),
    numberOfParameters = cms.uint32(10),
    args = cms.vstring('slc5_ia32_gcc434/madgraph/V5_1.4.8/8TeV_Summer12/Z_VBF_Lept_mqq_120_inf_8TeV_madgraph/v1',
                       'Z_VBF_Lept_mqq_120_inf_8TeV_madgraph',
                       'false','false','zjets','5','15','true','1','3'),
    nEvents = cms.uint32(30000)
)


