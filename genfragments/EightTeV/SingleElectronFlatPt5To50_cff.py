import FWCore.ParameterSet.Config as cms


generator = cms.EDProducer("FlatRandomPtGunProducer",
    PGunParameters = cms.PSet(
        MaxPt = cms.double(50.0),
        MinPt = cms.double(5.0),
        PartID = cms.vint32(11),
        MaxEta = cms.double(3.),
        MaxPhi = cms.double(3.14159265359),
        MinEta = cms.double(-3.),
        MinPhi = cms.double(-3.14159265359)
    ),
    Verbosity = cms.untracked.int32(0),
    psethack = cms.string('single electron pt 5 to 50'),
    AddAntiParticle = cms.bool(False),
    firstRun = cms.untracked.uint32(1)
)

