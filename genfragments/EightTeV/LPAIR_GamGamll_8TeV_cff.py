import FWCore.ParameterSet.Config as cms

from Configuration.Generator.PythiaUEZ2starSettings_cfi import *
generator = cms.EDFilter("LHEProducer",
                         hadronisation = cms.PSet(generator = cms.string('None'))
                         )








