import FWCore.ParameterSet.Config as cms
from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.Pythia8CUEP8M1Settings_cfi import *
from Configuration.Generator.Pythia8PowhegEmissionVetoSettings_cfi import *

generator = cms.EDFilter("Pythia8HadronizerFilter",
                         maxEventsToPrint = cms.untracked.int32(1),
                         pythiaPylistVerbosity = cms.untracked.int32(1),
                         filterEfficiency = cms.untracked.double(1.0),
                         pythiaHepMCVerbosity = cms.untracked.bool(False),
                         comEnergy = cms.double(13000.),
                         PythiaParameters = cms.PSet(
        pythia8CommonSettingsBlock,
        pythia8CUEP8M1SettingsBlock,
        pythia8PowhegEmissionVetoSettingsBlock,
        processParameters = cms.vstring(
#            'POWHEG:nFinal = 3',   ## Number of final state particles
#                                   ## (BEFORE THE DECAYS) in the LHE
#                                   ## other than emitted extra parton
            '6100027:all = S__plus__ S__minus__ 1 3 1 1100 1 0 1500', #charge 3*1?
            '6100027:oneChannel = 1  1.0 101  6 5',
            '6100027:mayDecay = on',
#            '6100027:isResonance = on',
            '6100027:tauCalc = on',

            '6100027:onMode = off',
            '6100027:onIfAny = 6 5',
            '6100027:doForceWidth = on' ## make it with a width
        ),
        parameterSets = cms.vstring('pythia8CommonSettings',
                                    'pythia8CUEP8M1Settings',
                                    'pythia8PowhegEmissionVetoSettings',
                                    'processParameters'
                                    )
        )
                         )

ProductionFilterSequence = cms.Sequence(generator)
