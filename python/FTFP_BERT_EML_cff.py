import FWCore.ParameterSet.Config as cms
def customise(process):

    process.g4SimHits.Physics.type = cms.string('SimG4Core/Physics/FTFP_BERT_EML')

    return(process)
