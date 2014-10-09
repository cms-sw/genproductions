import FWCore.ParameterSet.Config as cms

source = cms.Source("EmptySource")

generator = cms.EDFilter("Pythia8GeneratorFilter",
        comEnergy = cms.double(13000.0),
        crossSection = cms.untracked.double(4.471e-2),
        filterEfficiency = cms.untracked.double(1),
        maxEventsToPrint = cms.untracked.int32(0),
        pythiaHepMCVerbosity = cms.untracked.bool(False),
        pythiaPylistVerbosity = cms.untracked.int32(1),
        PythiaParameters = cms.PSet(
                processParameters = cms.vstring(
                        'Main:timesAllowErrors    = 10000',
                        'ParticleDecays:limitTau0 = on',
                        'ParticleDecays:tauMax = 10',
                        'Tune:pp 5',
                        'Tune:ee 3',

                        'ExtraDimensionsG*:all = on',
                        'ExtraDimensionsG*:kappaMG = 0.054',
                        '5100039:m0 = 750',
                        '5100039:onMode = off',
                        '5100039:onIfAny = 22',

                ),
                parameterSets = cms.vstring('processParameters')
        )
)

ProductionFilterSequence = cms.Sequence(generator)
