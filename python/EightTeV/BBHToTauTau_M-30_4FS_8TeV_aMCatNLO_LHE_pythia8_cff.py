import FWCore.ParameterSet.Config as cms

from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.Pythia8CUEP8M1Settings_cfi import *
from Configuration.Generator.Pythia8aMCatNLOSettings_cfi import *

generator = cms.EDFilter("Pythia8HadronizerFilter",
    maxEventsToPrint = cms.untracked.int32(1),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(8000.),
    PythiaParameters = cms.PSet(
        pythia8CommonSettingsBlock,
        pythia8CUEP8M1SettingsBlock,
        pythia8aMCatNLOSettingsBlock,
        processParameters = cms.vstring(
                        '25:m0 = 30.0',
                        '25:onMode = off', # turn OFF all h decays
                        '25:onIfMatch = 15 -15'
                        'SLHA:useDecayTable = off',
                        'TimeShower:nPartonsInBorn = 2', #number of coloured particles (before resonance decays) in born matrix element
            ),
        parameterSets = cms.vstring('pythia8CommonSettings',
            'pythia8CUEP8M1Settings',
            'pythia8aMCatNLOSettings',
            'processParameters',
        )
    )
)
