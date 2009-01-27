import FWCore.ParameterSet.Config as cms

source=cms.Source("SherpaSource",
  firstRun  = cms.untracked.uint32(1),
  libDir    = cms.untracked.string('SherpaRun'),
  resultDir = cms.untracked.string('Result')
)
