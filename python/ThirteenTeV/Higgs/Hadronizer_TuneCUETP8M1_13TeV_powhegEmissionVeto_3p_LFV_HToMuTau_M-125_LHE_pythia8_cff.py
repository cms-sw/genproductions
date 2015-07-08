import FWCore.ParameterSet.Config as cms
from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.Pythia8CUEP8M1Settings_cfi import *
from Configuration.Generator.Pythia8PowhegEmissionVetoSettings_cfi import *

generator = cms.EDFilter("Pythia8HadronizerFilter",
                       	comEnergy = cms.double(13000.0),
                       	filterEfficiency = cms.untracked.double(1),
                       	maxEventsToPrint = cms.untracked.int32(1),
                       	pythiaHepMCVerbosity = cms.untracked.bool(False),
                       	pythiaPylistVerbosity = cms.untracked.int32(1),
                       	PythiaParameters = cms.PSet(
      					pythia8CommonSettingsBlock,
      					pythia8CUEP8M1SettingsBlock,
      					pythia8PowhegEmissionVetoSettings = cms.vstring(
         					'POWHEG:veto = 1',
         					'POWHEG:pTdef = 1',
         					'POWHEG:emitted = 0',
         					'POWHEG:pTemt = 0',
         					'POWHEG:pThard = 0',
         					'POWHEG:vetoCount = 100',
         					'SpaceShower:pTmaxMatch = 2',
         					'TimeShower:pTmaxMatch = 2',
      						),
      					processParameters = cms.vstring(
                            			'POWHEG:nFinal = 3',                            
          					'TauDecays:mode = 2',
          					'TauDecays:tauPolarization = 0',
          					'TauDecays:tauMother = 25',
          					'6:m0 = 172.04',
          					'25:m0 = 125.0',
          					'25:addChannel 1 0.1 100 15 -13',
          					'25:addChannel 1 0.1 100 13 -15',
          					'25:onMode = off',
          					'25:onIfMatch 15 13'
          					),
      					parameterSets = cms.vstring(
                          			'pythia8PowhegEmissionVetoSettings',
                            			'pythia8CommonSettings',
                            			'pythia8CUEP8M1Settings',
                            			'processParameters',
                            			)
      					)
			)

ProductionFilterSequence = cms.Sequence(generator)
