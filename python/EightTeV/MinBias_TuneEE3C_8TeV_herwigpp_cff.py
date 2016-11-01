import FWCore.ParameterSet.Config as cms

from Configuration.Generator.HerwigppDefaults_cfi import *
from Configuration.Generator.HerwigppUE_EE_3C_cfi import *


generator = cms.EDFilter("ThePEGGeneratorFilter",
  herwigDefaultsBlock,
  herwigppUESettingsBlock,
  crossSection = cms.untracked.double(8.478808e+08),
  filterEfficiency = cms.untracked.double(1),

  configFiles = cms.vstring(),
  parameterSets = cms.vstring(
    'herwigppUE_EE_3C_8000GeV',
    'productionParameters',
    'basicSetup',
    'setParticlesStableForDetector',
  ),
  productionParameters = cms.vstring(
    'cd /Herwig/MatrixElements/',
    'insert SimpleQCD:MatrixElements[0] MEMinBias',

    'cd /Herwig/Cuts',
    'set JetKtCut:MinKT 0.0*GeV',
    'set QCDCuts:MHatMin 0.0*GeV',
    'set QCDCuts:X1Min 0.01',
    'set QCDCuts:X2Min 0.01',
    'set /Herwig/UnderlyingEvent/MPIHandler:IdenticalToUE 0',
  ),
)

configurationMetadata = cms.untracked.PSet(
  version = cms.untracked.string('\$Revision: 1.1 $'),
  name = cms.untracked.string('\$Source: /afs/cern.ch/project/cvs/reps/CMSSW/CMSSW/Configuration/GenProduction/python/EightTeV/QCD_Pt_15to3000_TuneEE3C_Flat_8TeV_herwigpp_cff.py,v $'),
  annotation = cms.untracked.string('Sumer2012 sample with HERWIGPP: MinBias, TuneEE3C')
)

