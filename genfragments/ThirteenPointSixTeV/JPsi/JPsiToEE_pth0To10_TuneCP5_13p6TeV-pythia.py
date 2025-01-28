import FWCore.ParameterSet.Config as cms

from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.MCTunesRun3ECM13p6TeV.PythiaCP5Settings_cfi import *
from Configuration.Generator.PSweightsPythia.PythiaPSweightsSettings_cfi import *

generator = cms.EDFilter("Pythia8ConcurrentGeneratorFilter",
                                 comEnergy = cms.double(13600.0),
                                 filterEfficiency = cms.untracked.double(1.0),
                                 maxEventsToPrint = cms.untracked.int32(0),
                                 pythiaPylistVerbosity = cms.untracked.int32(0),
                                 pythiaHepMCVerbosity = cms.untracked.bool(False),
                                 PythiaParameters = cms.PSet(

    pythia8CommonSettingsBlock,
    pythia8CP5SettingsBlock,
    pythia8PSweightsSettingsBlock,
    processParameters = cms.vstring(
      'Main:timesAllowErrors    = 10000',
      'Charmonium:gg2ccbar(3S1)[3S1(1)]g    = on,on',
      'Charmonium:gg2ccbar(3S1)[3S1(1)]gm   = on,on',
      'Charmonium:gg2ccbar(3S1)[3S1(8)]g    = on,on',
      'Charmonium:qg2ccbar(3S1)[3S1(8)]q    = on,on',
      'Charmonium:qqbar2ccbar(3S1)[3S1(8)]g = on,on',
      'Charmonium:gg2ccbar(3S1)[1S0(8)]g    = on,on',
      'Charmonium:qg2ccbar(3S1)[1S0(8)]q    = on,on',
      'Charmonium:qqbar2ccbar(3S1)[1S0(8)]g = on,on',
      'Charmonium:gg2ccbar(3S1)[3PJ(8)]g    = on,on',
      'Charmonium:qg2ccbar(3S1)[3PJ(8)]q    = on,on',
      'Charmonium:qqbar2ccbar(3S1)[3PJ(8)]g = on,on',
      'PhaseSpace:pTHatMax = 10.',
      '443:onMode = off',
      '443:onIfMatch = 11 -11',
      '100443:onMode = off',
      '100443:onIfMatch = 11 -11',
      ),

    parameterSets = cms.vstring(
      'pythia8CommonSettings',
      'pythia8CP5Settings',
      'pythia8PSweightsSettings',
      'processParameters')
    )
)


ProductionFilterSequence = cms.Sequence(generator)

