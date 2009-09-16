import FWCore.ParameterSet.Config as cms

generator = cms.EDProducer("LHEProducer",
        hadronisation = cms.PSet(
                generator = cms.string('None')
        )
)
