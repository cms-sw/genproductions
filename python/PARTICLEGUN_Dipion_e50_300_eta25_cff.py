import FWCore.ParameterSet.Config as cms


source = cms.Source("FlatRandomEGunSource",
    PGunParameters = cms.untracked.PSet(
        # you can request more than 1 particle
        PartID = cms.untracked.vint32(211),
        MinEta = cms.untracked.double(-2.5),
        MaxEta = cms.untracked.double(2.5),
        MinPhi = cms.untracked.double(-3.14159265359),
        MaxPhi = cms.untracked.double(3.14159265359),
        MinE   = cms.untracked.double(50.0),
        MaxE   = cms.untracked.double(300.0)
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
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PARTICLEGUN_Dipion_e50_300_eta25_cff.py,v $'),
    annotation = cms.untracked.string('di-pions of 50-300 GeV randomly distributed in the Tracker acceptance')
)
