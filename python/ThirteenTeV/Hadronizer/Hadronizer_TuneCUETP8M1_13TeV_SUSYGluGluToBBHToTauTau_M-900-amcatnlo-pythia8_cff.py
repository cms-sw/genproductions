import FWCore.ParameterSet.Config as cms

from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.Pythia8CUEP8M1Settings_cfi import *
from Configuration.Generator.Pythia8aMCatNLOSettings_cfi import *
from Configuration.Generator.PSweightsPythia.PythiaPSweightsSettings_cfi import *

generator = cms.EDFilter("Pythia8HadronizerFilter",
    maxEventsToPrint = cms.untracked.int32(1),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(13000.),
    PythiaParameters = cms.PSet(
          pythia8CommonSettingsBlock,
          pythia8CUEP8M1SettingsBlock,
          pythia8aMCatNLOSettingsBlock,
          pythia8PSweightsSettingsBlock,
          processParameters = cms.vstring(
            'TimeShower:nPartonsInBorn = 2', #number of coloured particles (before resonance decays) in born matrix element
            'SLHA:useDecayTable = off',
            '25:onMode = off', # turn OFF all H decays
            '25:onIfAny = 15',    # turn ON H->tautau
            '25:m0 = 900'         # mass of H
           ),
         parameterSets = cms.vstring('pythia8CommonSettings',
        'pythia8CUEP8M1Settings',
        'pythia8aMCatNLOSettings',
        'pythia8PSweightsSettings',
        'processParameters'
        )
    )
)

