import FWCore.ParameterSet.Config as cms


source = cms.Source("FlatRandomEGunSource",
    PGunParameters = cms.untracked.PSet(
        # you can request more than 1 particle
        PartID = cms.untracked.vint32(211),
        MaxEta = cms.untracked.double(5.1),
        MaxPhi = cms.untracked.double(3.14159265359),
        MinEta = cms.untracked.double(-5.1),
        MinE   = cms.untracked.double(7.0),
        MinPhi = cms.untracked.double(-3.14159265359),
        MaxE   = cms.untracked.double(7.0)
    ),
    firstRun = cms.untracked.uint32(1),
    AddAntiParticle = cms.untracked.bool(True) 
)

VtxSmeared = cms.EDFilter("GaussEvtVtxGenerator",
    MeanX = cms.double(0.0),
    MeanY = cms.double(0.0),
    MeanZ = cms.double(0.0),
    SigmaY = cms.double(0.0001),
    SigmaX = cms.double(0.0001),
    SigmaZ = cms.double(0.0001),
    TimeOffset = cms.double(0.0),
    src = cms.InputTag("source")
)

# enter below the configuration metadata (only a description is needed, the rest is filled in by cvs)
configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: $'),
    name = cms.untracked.string('$Source: $'),
    annotation = cms.untracked.string('di-pion of 7 GeV randomly distributed in the entire Calorimetry')
)
