import FWCore.ParameterSet.Config as cms

generator = cms.EDProducer("FlatRandomPtGunProducer",
    PGunParameters = cms.PSet(
        MaxPt = cms.double(2.),
        MinPt = cms.double(250.),
        PartID = cms.vint32(11),
        MaxEta = cms.double(2.5),
        MaxPhi = cms.double(3.14159265359),
        MinEta = cms.double(-2.5),
        MinPhi = cms.double(-3.14159265359) ## in radians

    ),
    Verbosity = cms.untracked.int32(0), ## set to 1 (or greater)  for printouts

    psethack = cms.string('single electron pt 2 to 250'),
    AddAntiParticle = cms.bool(True),
    firstRun = cms.untracked.uint32(1)
)

configurationMetadata = cms.untracked.PSet(
        version = cms.untracked.string('\$Revision: 1.1 $'),
        name = cms.untracked.string('\$Source: /cvs/CMSSW/CMSSW/Configuration/GenProduction/python/SingleElectronFlatPt5To100_cff.py,v $'),
        annotation = cms.untracked.string('Summer2011 sample with GUN: Flat Electron gun, pt = 2 .. 250 GeV, ')
)
