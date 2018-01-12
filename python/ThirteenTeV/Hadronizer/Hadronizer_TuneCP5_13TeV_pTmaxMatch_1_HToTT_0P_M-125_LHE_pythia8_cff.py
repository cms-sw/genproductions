import FWCore.ParameterSet.Config as cms

from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.MCTunes2017.PythiaCP5Settings_cfi import *

generator = cms.EDFilter("Pythia8HadronizerFilter",
    maxEventsToPrint = cms.untracked.int32(1),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(13000.),
    UseExternalGenerators = cms.untracked.bool(True),
    ExternalDecays = cms.PSet(
        Tauola = cms.untracked.PSet(
            UseTauolaPolarization = cms.bool(True),
            InputCards = cms.PSet(
                mdtau = cms.int32(0),
                pjak2 = cms.int32(0),
                pjak1 = cms.int32(0)
            ),
            parameterSets = cms.vstring('setHiggsScalarPseudoscalarPDG', 'setHiggsScalarPseudoscalarMixingAngle'),
            setHiggsScalarPseudoscalarPDG = cms.int32(25),
            setHiggsScalarPseudoscalarMixingAngle = cms.double(0)
        ),
        parameterSets = cms.vstring('Tauola'),
    ),
    PythiaParameters = cms.PSet(
        pythia8CommonSettingsBlock,
        pythia8CP5SettingsBlock,
        processParameters = cms.vstring(
            'SpaceShower:pTmaxMatch = 1',
            'TimeShower:pTmaxMatch = 1',
            'SpaceShower:pTmaxFudge = 1',
            'TimeShower:pTmaxFudge = 1',
            '25:onMode = off',       # turn OFF all H decays
            '25:onIfMatch = 15 -15', # turn ON H->tautau
            '25:m0 = 125.0',
        ),
        parameterSets = cms.vstring('pythia8CommonSettings',
                                    'pythia8CP5Settings',
                                    'processParameters',
                                    )
    )
)

