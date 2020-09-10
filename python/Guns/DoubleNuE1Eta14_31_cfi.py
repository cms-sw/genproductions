import FWCore.ParameterSet.Config as cms

generator = cms.EDProducer("FlatRandomEGunProducer",
    PGunParameters = cms.PSet(
        MaxE = cms.double(1.01),
        MinE = cms.double(0.99),
        PartID = cms.vint32(12),
        MaxEta = cms.double(3.1),
        MaxPhi = cms.double(3.14159265359),
        MinEta = cms.double(1.4),
        MinPhi = cms.double(-3.14159265359) ## in radians

    ),
    Verbosity = cms.untracked.int32(0), ## set to 1 (or greater)  for printouts

    psethack = cms.string('single nu E 1'),
    AddAntiParticle = cms.bool(True),
    firstRun = cms.untracked.uint32(1)
)

