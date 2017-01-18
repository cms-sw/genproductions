import FWCore.ParameterSet.Config as cms

def customise(process):

    # extended geometric acceptance (full CASTOR acceptance)

    process.g4SimHits.Generator.MinEtaCut = cms.double(-7.7)
    process.g4SimHits.CastorSD.nonCompensationFactor = cms.double(0.77)

    return(process)
