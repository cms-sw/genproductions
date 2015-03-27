import FWCore.ParameterSet.Config as cms

from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.Pythia8CUEP8M1Settings_cfi import *

generator = cms.EDFilter("Pythia8GeneratorFilter",
                         maxEventsToPrint = cms.untracked.int32(1),
                         pythiaPylistVerbosity = cms.untracked.int32(1),
                         filterEfficiency = cms.untracked.double(1.0),
                         pythiaHepMCVerbosity = cms.untracked.bool(False),
                         comEnergy = cms.double(13000.),
                         PythiaParameters = cms.PSet(
                                     pythia8CommonSettingsBlock,
                                     pythia8CUEP8M1SettingsBlock,
                                     processParameters = cms.vstring(
                                             'Main:timesAllowErrors = 10000',
                                             'ExtraDimensionsUnpart:ffbar2UZ = on',
                                             'ParticleDecays:limitTau0 = on',
                                             'ParticleDecays:tau0Max = 10.',
                                             'ExtraDimensionsUnpart:spinU = 0',
                                             'ExtraDimensionsUnpart:dU = 1.30',
                                             'ExtraDimensionsUnpart:LambdaU = 15000.000000',
                                             'ExtraDimensionsUnpart:lambda = 1.0',
                                             'ExtraDimensionsUnpart:CutOffmode = 0',
                                             '5000039:m0 = 500.',
                                             '5000039:mWidth = 1000.',
                                             '5000039:mMin = 1.',
                                             '5000039:mMax = 13990.',
                                             'SecondHard:generate = off',
                                             'SecondHard:TwoJets = off',
                                             'PhaseSpace:sameForSecond = off',
                                             'PhaseSpace:pTHatMin = 30.',
                                             '23:onMode=off',
                                             '23:mMin=50',
					     '23:onIfAny=11 13'
                                             ),
                                     parameterSets = cms.vstring('pythia8CommonSettings',
                                             'pythia8CUEP8M1Settings',
                                             'processParameters')
                                 )
                        )
