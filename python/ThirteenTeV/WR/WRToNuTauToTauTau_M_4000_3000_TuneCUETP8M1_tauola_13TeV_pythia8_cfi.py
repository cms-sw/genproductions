import FWCore.ParameterSet.Config as cms

from GeneratorInterface.ExternalDecays.TauolaSettings_cff import *
from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.Pythia8CUEP8M1Settings_cfi import *

generator = cms.EDFilter("Pythia8GeneratorFilter",
    maxEventsToPrint = cms.untracked.int32(1),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(13000.),
    ExternalDecays = cms.PSet(Tauola = cms.untracked.PSet(TauolaPolar, TauolaDefaultInputCards ),
                                                parameterSets = cms.vstring('Tauola')
                              ),
    PythiaParameters = cms.PSet(
        pythia8CommonSettingsBlock,
        pythia8CUEP8M1SettingsBlock,
        processParameters = cms.vstring(
            'LeftRightSymmmetry:ffbar2WR = on',
            '9900024:m0 = 4000',
            '9900016:m0 = 3000',
            '9900024:onMode = off',
            '9900024:onIfAny = 9900016',
        ),
        parameterSets = cms.vstring('pythia8CommonSettings',
                                    'pythia8CUEP8M1Settings',
                                    'processParameters',
                                    )
    )
)
