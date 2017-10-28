import FWCore.ParameterSet.Config as cms

from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.Pythia8CUEP8M1Settings_cfi import *

generator = cms.EDFilter("Pythia8GeneratorFilter",
	comEnergy = cms.double(13000.0),
	pythiaHepMCVerbosity = cms.untracked.bool(False),
	pythiaPylistVerbosity = cms.untracked.int32(1),
	PythiaParameters = cms.PSet(
                pythia8CommonSettingsBlock,
                pythia8CUEP8M1SettingsBlock,
                processParameters = cms.vstring(
                    'HiddenValley:ffbar2Zv = on',
                    'HiddenValley:Ngauge = 3',
                    '4900023:mWidth = 0.01',
                    'HiddenValley:pTminFSR = .1',
                    'HiddenValley:alphaFSR = .8',
                    'HiddenValley:FSR = on',
                    'HiddenValley:fragment = on',
                    'HiddenValley:probVector = 0',
                    'PartonLevel:MPI = on',
                    'PartonLevel:ISR = on',
                    'PartonLevel:FSR = on',
                    'HadronLevel:Hadronize = on',
                    '4900023:onMode = off',
                    '4900023:onIfAny = 4900101',
                    '4900023:m0 = 500', #Z' mass
                    '4900101:m0 = .5',
                    '4900111:m0 = 20', #Dark Pion Mass
                    '4900111:mayDecay = on',
                    '4900111:addChannel 1 1. 0 22 22', #force dark pion to decay to diphotons
                    '4900111:tau0 = 2500', #Dark pion lifetime in mm
                    '4900211:mayDecay = off',
                    '-4900211:mayDecay = off'
                ),
                parameterSets = cms.vstring('pythia8CommonSettings',
                                            'pythia8CUEP8M1Settings',
                                            'processParameters')
       )
)

ProductionFilterSequence = cms.Sequence(generator)
