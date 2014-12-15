import FWCore.ParameterSet.Config as cms
from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.Pythia8CUEP8M1Settings_cfi import *

generator = cms.EDFilter("Pythia8HadronizerFilter",
                         maxEventsToPrint = cms.untracked.int32(1),
                         pythiaPylistVerbosity = cms.untracked.int32(1),
                         filterEfficiency = cms.untracked.double(1.0),
                         pythiaHepMCVerbosity = cms.untracked.bool(False),
                         comEnergy = cms.double(13000.),
                         PythiaParameters = cms.PSet(
        pythia8CommonSettingsBlock,
        pythia8CUEP8M1SettingsBlock,
        processParameters = cms.vstring(
            'POWHEG:veto = 1',
            'POWHEG:nFinal = 1',
            'POWHEG:pTdef = 1',
            'POWHEG:emitted = 0',
            'POWHEG:pTemt = 0',
            'POWHEG:pThard = 0',
            'POWHEG:vetoCount = 100',
            'SpaceShower:pTmaxMatch = 2',
            'TimeShower:pTmaxMatch = 2',

         ),
        parameterSets = cms.vstring('pythia8CommonSettings',
                                    'pythia8CUEP8M1Settings',
                                    'processParameters'
                                    )
        )
                         )

ProductionFilterSequence = cms.Sequence(generator)
