import FWCore.ParameterSet.Config as cms

from GeneratorInterface.ExternalDecays.TauolaSettings_cff import *
generator = cms.EDFilter("Pythia8HadronizerFilter",
                         ExternalDecays = cms.PSet(
    Tauola = cms.untracked.PSet(
    TauolaPolar,
    TauolaDefaultInputCards
    ),
    parameterSets = cms.vstring('Tauola')
    ),
    UseExternalGenerators = cms.untracked.bool(True),
    maxEventsToPrint = cms.untracked.int32(1),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(13000.),
    PythiaParameters = cms.PSet(
        processParameters = cms.vstring(
	'Main:timesAllowErrors    = 10000', 
        'ParticleDecays:limitTau0 = on',
	'ParticleDecays:tau0Max = 10',
        'Tune:ee 3',
        'Tune:pp 5',
        '25:onMode = off',      # turn OFF all H decays
        '25:onIfMatch = 23 23',      # turn ON H->ZZ
        '23:onMode = off',      # turn OFF all Z decays
        '23:onIfAny = 11 13 15', # turn ON Z->ll 
        ),
        parameterSets = cms.vstring('processParameters')
    )
)
