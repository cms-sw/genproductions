import FWCore.ParameterSet.Config as cms

from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.Pythia8CUEP8M1Settings_cfi import *

process.generator = cms.EDFilter("Pythia8GeneratorFilter",
                                 comEnergy = cms.double(13000.0),
                                 crossSection = cms.untracked.double(8.45E-6),
                                 filterEfficiency = cms.untracked.double(1),
                                 maxEventsToPrint = cms.untracked.int32(1),
                                 pythiaPylistVerbosity = cms.untracked.int32(1),
                                 filterEfficiency = cms.untracked.double(1.0),
                                 pythiaHepMCVerbosity = cms.untracked.bool(False),
                                 PythiaParameters = cms.PSet(

    pythia8CommonSettingsBlock,
    pythia8CUEP8M1SettingsBlock,
    processParameters = cms.vstring(
      'Main:timesAllowErrors    = 10000',
      'HiggsSM:all=true',
      '25:m0 = 125.0',
      '25:onMode = off',
      '25:addChannel = 1  1.00   103   22   443',
      '443:onMode = off',
      '443:addChannel = 1 1. 0 13 -13',
      ),

    parameterSets = cms.vstring(
      'pythia8CommonSettings',
      'pythia8CUEP8M1Settings',
      'processParameters')
    )
                                 )
ProductionFilterSequence = cms.Sequence(generator)
