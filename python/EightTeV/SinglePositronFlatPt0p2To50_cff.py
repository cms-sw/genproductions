import FWCore.ParameterSet.Config as cms


generator = cms.EDProducer("FlatRandomPtGunProducer",
    PGunParameters = cms.PSet(
        MaxPt = cms.double(50.0),
        MinPt = cms.double(0.2),
        PartID = cms.vint32(-11),
        MaxEta = cms.double(3.),
        MaxPhi = cms.double(3.14159265359),
        MinEta = cms.double(-3.),
        MinPhi = cms.double(-3.14159265359)
    ),
    Verbosity = cms.untracked.int32(0),
    psethack = cms.string('single positron pt 0.2 to 50'),
    AddAntiParticle = cms.bool(False),
    firstRun = cms.untracked.uint32(1)
)

