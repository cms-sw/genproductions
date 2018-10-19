import FWCore.ParameterSet.Config as cms
from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.MCTunes2017.PythiaCP2Settings_cfi import *

generator = cms.EDFilter("Pythia8HadronizerFilter",
                         maxEventsToPrint = cms.untracked.int32(1),
                         pythiaPylistVerbosity = cms.untracked.int32(1),
                         filterEfficiency = cms.untracked.double(1.0),
                         pythiaHepMCVerbosity = cms.untracked.bool(False),
                         comEnergy = cms.double(13000.),
                         PythiaParameters = cms.PSet(
                             pythia8CommonSettingsBlock,
                             pythia8CP2SettingsBlock,
                             processParameters = cms.vstring(
                                 'SLHA:useDecayTable = off',  # Use pythia8s own decay mode instead of decays defined in LH accord
                                 '24:mMin = 0.05',                  
                                 '24:onMode = on',                 
                                 '25:m0 = 125.0',
                                 '25:onMode = off',
                                 '25:onIfMatch = 5 -5'
                                 '25:onIfMatch = 24 -24',           # turn ON H->WW
                                 'ResonanceDecayFilter:filter = on',
                                 'ResonanceDecayFilter:exclusive = on', #off: require at least the specified number of daughters, on: require exactly the specified number of daughters
                                 'ResonanceDecayFilter:mothers = 25', #list of mothers not specified -> count all particles in hard process+resonance decays (better to avoid specifying mothers when including leptons from the lhe in counting, since intermediate resonances are not gauranteed to appear in general
                                 'ResonanceDecayFilter:daughters = 5,5,24,-24'
                                 ),
                             parameterSets = cms.vstring('pythia8CommonSettings',
                                                         'pythia8CP2Settings',
                                                         'processParameters'
                                                         )
                             )
                         )

ProductionFilterSequence = cms.Sequence(generator)
