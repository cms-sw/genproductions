import FWCore.ParameterSet.Config as cms

generator = cms.EDProducer("FlatRandomEGunProducer",
    PGunParameters = cms.PSet(
        MaxE = cms.double(1000.01),
        MinE = cms.double(999.99),
        PartID = cms.vint32(11),
        MaxEta = cms.double(2.8),
        MaxPhi = cms.double(3.14159265359),
        MinEta = cms.double(1.6),
        MinPhi = cms.double(-3.14159265359) ## in radians

    ),
    Verbosity = cms.untracked.int32(0), ## set to 1 (or greater)  for printouts

    psethack = cms.string('single electron E 1000'),
    AddAntiParticle = cms.bool(True),
    firstRun = cms.untracked.uint32(1)
)

