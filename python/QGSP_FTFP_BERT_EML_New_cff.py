import FWCore.ParameterSet.Config as cms
def customiseGeant4(process):

    process.g4SimHits.Physics.type = cms.string('SimG4Core/Physics/QGSP_FTFP_BERT_EML_New')

    return(process)
