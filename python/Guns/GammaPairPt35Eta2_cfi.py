import FWCore.ParameterSet.Config as cms

generator = cms.EDProducer("MultiParticleInConeGunProducer",
    PGunParameters = cms.PSet(
        PartID = cms.vint32(22),
        MaxPt = cms.double(35.01),
        MinPt = cms.double(34.99),
        MinEta = cms.double(1.99999),
        MaxEta = cms.double(2.00001),
        MinPhi = cms.double(0.),
        MaxPhi = cms.double(1.570796327*4),
        InConeID = cms.vint32(22),
        MinDeltaR = cms.double(0.0),
        MaxDeltaR = cms.double(0.1),
        MinMomRatio = cms.double(1.),
        MaxMomRatio = cms.double(1.),
        InConeMinEta = cms.double(1.9),
        InConeMaxEta = cms.double(2.1),
        InConeMinPhi = cms.double(0.),
        InConeMaxPhi = cms.double(1.570796327*4),
        InConeMaxTry = cms.uint32(10)
    ),
    Verbosity = cms.untracked.int32(0),
    psethack = cms.string('single photon pt 35'),
    AddAntiParticle = cms.bool(False),
    firstRun = cms.untracked.uint32(1)
)
