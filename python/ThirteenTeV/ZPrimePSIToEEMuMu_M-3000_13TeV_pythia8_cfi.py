import FWCore.ParameterSet.Config as cms

source = cms.Source("EmptySource")

generator = cms.EDFilter("Pythia8GeneratorFilter",
        comEnergy = cms.double(13000.0),
        crossSection = cms.untracked.double(1e10),
        filterEfficiency = cms.untracked.double(1),
        maxEventsToPrint = cms.untracked.int32(0),
        pythiaHepMCVerbosity = cms.untracked.bool(False),
        pythiaPylistVerbosity = cms.untracked.int32(1),
        PythiaParameters = cms.PSet(
                processParameters = cms.vstring(
                        'Main:timesAllowErrors    = 10000',
                        'ParticleDecays:limitTau0 = on',
                        'ParticleDecays:tauMax = 10',
                        'Tune:pp 8',
                        'Tune:ee 3',

                        'NewGaugeBoson:ffbar2gmZZPrime = on',
                        'Zprime:gmZmode = 3',
                        'Zprime:universality = on',
                        'Zprime:vd=0',
                        'Zprime:ad=0.506809',
                        'Zprime:vu=0',
                        'Zprime:au=0.506809',
                        'Zprime:ve=0',
                        'Zprime:ae=0.506809',
                        'Zprime:vnue=-0.253405',
                        'Zprime:anue=0.253405',
                        '32:m0 = 3000',
                        '32:onMode = off',
                        '32:onIfAny = 11 13',

                ),
                parameterSets = cms.vstring('processParameters')
        )
)

ProductionFilterSequence = cms.Sequence(generator)
