import FWCore.ParameterSet.Config as cms

from Configuration.Generator.PythiaUESettings_cfi import *
generator = cms.EDFilter("LHEProducer",
                         hadronisation = cms.PSet(generator = cms.string('None'))
                         )

