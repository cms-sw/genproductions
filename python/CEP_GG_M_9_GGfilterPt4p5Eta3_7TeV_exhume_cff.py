import FWCore.ParameterSet.Config as cms

from GeneratorInterface.ExhumeInterface.ExhumeParameters_cfi import ExhumeParameters as ExhumeParametersRef

source = cms.Source("EmptySource")

generator = cms.EDFilter("ExhumeGeneratorFilter",
    PythiaParameters = cms.PSet(
       parameterSets = cms.vstring()
    ),
    ExhumeParameters = ExhumeParametersRef,
    comEnergy = cms.double(7000.),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    maxEventsToPrint = cms.untracked.int32(2),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    ExhumeProcess = cms.PSet(
       ProcessType = cms.string('DiPhoton'),
       ThetaMin = cms.double(0.30),
       MassRangeLow = cms.double(9.0),
       MassRangeHigh = cms.double(99999.0)
    )
)

gamgamgenfilter = cms.EDFilter("MCParticlePairFilter",
    Status = cms.untracked.vint32(1, 1),
    MinPt = cms.untracked.vdouble(4.5, 4.5),
    MaxEta = cms.untracked.vdouble(3, 3),
    MinEta = cms.untracked.vdouble(-3, -3),
    ParticleCharge = cms.untracked.int32(0),
    ParticleID1 = cms.untracked.vint32(22),
    ParticleID2 = cms.untracked.vint32(22)
)


ProductionFilterSequence = cms.Sequence(generator*gamgamgenfilter)

