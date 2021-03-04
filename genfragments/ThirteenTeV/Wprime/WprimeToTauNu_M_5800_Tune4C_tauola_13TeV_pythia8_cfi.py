import FWCore.ParameterSet.Config as cms

source = cms.Source("EmptySource")

from GeneratorInterface.ExternalDecays.TauolaSettings_cff import *

generator = cms.EDFilter("Pythia8GeneratorFilter",
        comEnergy = cms.double(13000.0),
        crossSection = cms.untracked.double(1.713e-04),
        filterEfficiency = cms.untracked.double(1),
        maxEventsToPrint = cms.untracked.int32(0),
        pythiaHepMCVerbosity = cms.untracked.bool(False),
        pythiaPylistVerbosity = cms.untracked.int32(1),
        ExternalDecays = cms.PSet(
                Tauola = cms.untracked.PSet(
                        TauolaPolar,
                        TauolaDefaultInputCards
                ),
                parameterSets = cms.vstring('Tauola')
        ),
        PythiaParameters = cms.PSet(
                processParameters = cms.vstring(
                        'Main:timesAllowErrors = 10000',
                        #'ParticleDecays:limitTau0 = on',
                        #'ParticleDecays:tauMax = 10',
                        'Tune:ee 3',
                        'Tune:pp 5',

                        'NewGaugeBoson:ffbar2Wprime = on',
                        '34:m0 = 5800',
                        '34:onMode = off',
                        '34:onIfAny = 15,16',
                        '15:onMode = off',
                ),
                parameterSets = cms.vstring('processParameters')
        )
)

#ProductionFilterSequence = cms.Sequence(generator)
