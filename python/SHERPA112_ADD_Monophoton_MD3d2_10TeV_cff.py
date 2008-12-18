import FWCore.ParameterSet.Config as cms

source=cms.Source("SherpaSource",
  firstRun  = cms.untracked.uint32(1),
  libDir    = cms.untracked.string('SherpaRun'),
  resultDir = cms.untracked.string('Result')
)

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision 1.1. %'),
    annotation = cms.untracked.string('default documentation string for SHERPA112_ADD_Monophoton_MD3d2_10TeV_cff.py'),
    name = cms.untracked.string('$Source: /local/projects/CMSSW/rep/CMSSW/Configuration/GenProduction/python/SHERPA112_ADD_Monophoton_MD3d2_10TeV_cff.py,v $')
)
