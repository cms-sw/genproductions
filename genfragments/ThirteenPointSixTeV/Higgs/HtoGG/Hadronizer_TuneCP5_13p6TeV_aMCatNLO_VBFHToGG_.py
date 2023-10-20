from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.MCTunesRun3ECM13p6TeV.PythiaCP5Settings_cfi import *
from Configuration.Generator.PSweightsPythia.PythiaPSweightsSettings_cfi import *
from Configuration.Generator.Pythia8aMCatNLOSettings_cfi import *

generator = cms.EDFilter("Pythia8ConcurrentHadronizerFilter",
	                     maxEventsToPrint = cms.untracked.int32(1),
                         pythiaPylistVerbosity = cms.untracked.int32(1),
                         filterEfficiency = cms.untracked.double(1.0),
                         pythiaHepMCVerbosity = cms.untracked.bool(False),
                         comEnergy = cms.double(13600.),
                         PythiaParameters = cms.PSet(
     pythia8CommonSettingsBlock,
     pythia8CP5SettingsBlock,
     pythia8PSweightsSettingsBlock,
     pythia8aMCatNLOSettingsBlock,
     processParameters = cms.vstring(
         'TimeShower:nPartonsInBorn = 2', #number of coloured particles (before resonance decays) in born matrix element
         'SLHA:useDecayTable = off',
         '25:m0 = <MASS>',
         '25:onMode = off',
         '25:onIfMatch = 22 22',
         ),
     parameterSets = cms.vstring('pythia8CommonSettings',
                                 'pythia8CP5Settings',
                                 'pythia8PSweightsSettings',
                                 'pythia8aMCatNLOSettings',
                                 'processParameters',
                                 )
     )
)
