import FWCore.ParameterSet.Config as cms

generator = cms.EDProducer("FlatRandomPtGunProducer",
    PGunParameters = cms.PSet(
        MaxPt = cms.double(250.),
        MinPt = cms.double(2.),
        PartID = cms.vint32(22),
        MaxEta = cms.double(2.5),
        MaxPhi = cms.double(3.14159265359),
        MinEta = cms.double(-2.5),
        MinPhi = cms.double(-3.14159265359) ## in radians

    ),
    Verbosity = cms.untracked.int32(0), ## set to 1 (or greater)  for printouts

    psethack = cms.string('single gamma pt 2 to 250'),
    AddAntiParticle = cms.bool(False),
    firstRun = cms.untracked.uint32(1)
)

configurationMetadata = cms.untracked.PSet(
        version = cms.untracked.string('\$Revision: 1.1 $'),
        name = cms.untracked.string('\$Source: /cvs/CMSSW/CMSSW/Configuration/GenProduction/python/SingleGamma_Pt_2To250_cff.py,v $'),
        annotation = cms.untracked.string('Summer2011 sample with GUN: Flat Gamma gun, pt = 2 .. 250 GeV, ')
)
