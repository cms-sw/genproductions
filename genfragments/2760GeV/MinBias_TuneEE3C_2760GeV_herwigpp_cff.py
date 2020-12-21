import FWCore.ParameterSet.Config as cms

from Configuration.Generator.HerwigppDefaults_cfi import *
from Configuration.Generator.HerwigppUE_EE_3C_cfi import *


generator = cms.EDFilter("ThePEGGeneratorFilter",
  herwigDefaultsBlock,
  herwigppUESettingsBlock,
  crossSection = cms.untracked.double(-1),
  filterEfficiency = cms.untracked.double(1),

  configFiles = cms.vstring(),
  parameterSets = cms.vstring(
    'herwigppUE_EE_3C_2760GeV',
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
