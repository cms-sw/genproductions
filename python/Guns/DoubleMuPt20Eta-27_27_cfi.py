import FWCore.ParameterSet.Config as cms

generator = cms.EDProducer("FlatRandomPtGunProducer",
    PGunParameters = cms.PSet(
        MaxPt = cms.double(20.01),
        MinPt = cms.double(19.99),
        PartID = cms.vint32(13),
        MaxEta = cms.double(2.7),
        MaxPhi = cms.double(3.14159265359),
        MinEta = cms.double(-2.7),
        MinPhi = cms.double(-3.14159265359) ## in radians

    ),
    Verbosity = cms.untracked.int32(2), ## set to 1 (or greater)  for printouts

    psethack = cms.string('single mu pt 20'),
    AddAntiParticle = cms.bool(True),
    firstRun = cms.untracked.uint32(1)
)
