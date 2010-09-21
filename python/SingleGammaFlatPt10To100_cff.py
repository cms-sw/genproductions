import FWCore.ParameterSet.Config as cms

generator = cms.EDProducer("FlatRandomPtGunProducer",
    PGunParameters = cms.PSet(
        MaxPt = cms.double(100.),
        MinPt = cms.double(10.),
        PartID = cms.vint32(22),
        MaxEta = cms.double(2.5),
        MaxPhi = cms.double(3.14159265359),
        MinEta = cms.double(-2.5),
        MinPhi = cms.double(-3.14159265359) ## in radians

    ),
    Verbosity = cms.untracked.int32(0), ## set to 1 (or greater)  for printouts

    psethack = cms.string('single gamma pt 10 to 100'),
    AddAntiParticle = cms.bool(False),
    firstRun = cms.untracked.uint32(1)
)

configurationMetadata = cms.untracked.PSet(
        version = cms.untracked.string('\$Revision: 1.1 $'),
        name = cms.untracked.string('\$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/SingleGammaFlatPt10To100_cff.py,v $'),
        annotation = cms.untracked.string('Fall2010 sample with GUN: Flat Gamma gun, pt = 10 .. 100 GeV, ')
)
