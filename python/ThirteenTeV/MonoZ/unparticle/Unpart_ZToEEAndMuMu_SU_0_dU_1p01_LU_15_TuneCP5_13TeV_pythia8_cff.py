import FWCore.ParameterSet.Config as cms

from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.MCTunes2017.PythiaCP5Settings_cfi import *
from Configuration.Generator.PSweightsPythia.PythiaPSweightsSettings_cfi import *

generator = cms.EDFilter("Pythia8GeneratorFilter",
                         maxEventsToPrint = cms.untracked.int32(1),
                         pythiaPylistVerbosity = cms.untracked.int32(1),
                         filterEfficiency = cms.untracked.double(1.0),
                         pythiaHepMCVerbosity = cms.untracked.bool(False),
                         comEnergy = cms.double(13000.),
                         PythiaParameters = cms.PSet(
                                     pythia8CommonSettingsBlock,
                                     pythia8CP5SettingsBlock,
                                     pythia8PSweightsSettingsBlock,
                                     processParameters = cms.vstring(

                                             'ExtraDimensionsUnpart:ffbar2UZ = on',


                                             'ExtraDimensionsUnpart:spinU = 0',
                                             'ExtraDimensionsUnpart:dU = 1.01',
                                             'ExtraDimensionsUnpart:LambdaU = 15000.000000',
                                             'ExtraDimensionsUnpart:lambda = 1.0',
                                             'ExtraDimensionsUnpart:CutOffmode = 0',
                                             '5000039:m0 = 500.',
                                             '5000039:mWidth = 1000.',
                                             '5000039:mMin = 1.',
                                             '5000039:mMax = 13990.',
                                             'PhaseSpace:sameForSecond = off',
                                             'PhaseSpace:pTHatMin = 30.',
                                             '23:onMode=off',
                                             '23:mMin=50',
					     '23:onIfAny=11 13'
                                             ),
                                     parameterSets = cms.vstring('pythia8CommonSettings',
                                             'pythia8CP5Settings',
                                             'processParameters',
                                             'pythia8PSweightsSettings')
                                 )
                        )
