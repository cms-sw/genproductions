import FWCore.ParameterSet.Config as cms

generator = cms.EDFilter("Pythia8GeneratorFilter",
       comEnergy = cms.double(14000.0),
       crossSection = cms.untracked.double(1.),
       filterEfficiency = cms.untracked.double(1),
       maxEventsToPrint = cms.untracked.int32(0),
       pythiaHepMCVerbosity = cms.untracked.bool(False),
       pythiaPylistVerbosity = cms.untracked.int32(0),

       PythiaParameters = cms.PSet(
              processParameters = cms.vstring(
                     'Main:timesAllowErrors = 10000',
                     'ParticleDecays:limitTau0 = on',
                     'ParticleDecays:tauMax = 10',
                     'HardQCD:gg2bbbar = on',
                     'HardQCD:qqbar2bbbar = on',
                     'PhaseSpace:pTHatMin = 15',
                     'PhaseSpace:pTHatMax = 3000',
                     'Tune:pp 5',
                     'Tune:ee 3'
              ),
              parameterSets = cms.vstring('processParameters')
       )
)

mmfltr = cms.EDFilter("MCParticlePairFilter",
    MinP = cms.untracked.vdouble(2.5, 2.5),
    MaxEta = cms.untracked.vdouble(3.0, 3.0),
    MinEta = cms.untracked.vdouble(-3.0, -3.0),
    MaxDeltaR = cms.untracked.double(1.2),
    ParticleID1 = cms.untracked.vint32(13),
    ParticleID2 = cms.untracked.vint32(13)
)

ProductionFilterSequence = cms.Sequence(generator * mmfltr)
