import FWCore.ParameterSet.Config as cms

generator = cms.EDFilter("LHEProducer",
        hadronisation = cms.PSet(
                generator = cms.string('None')
        )
)
