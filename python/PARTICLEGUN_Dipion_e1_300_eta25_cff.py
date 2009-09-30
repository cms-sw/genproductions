import FWCore.ParameterSet.Config as cms

source = cms.Source("EmptySource")
from Configuration.GenProduction.PythiaUESettings_cfi import *

generator = cms.EDProducer("FlatRandomEGunProducer",
    PGunParameters = cms.PSet(
        # you can request more than 1 particle
        PartID = cms.vint32(211),
        MinEta = cms.double(-2.5),
        MaxEta = cms.double(2.5),
        MinPhi = cms.double(-3.14159265359),
        MaxPhi = cms.double(3.14159265359),
        MinE   = cms.double(1.0),
        MaxE   = cms.double(300.0)
    ),
    firstRun = cms.untracked.uint32(1),
    AddAntiParticle = cms.bool(True) 
)



# enter below the configuration metadata (only a description is needed, the rest is filled in by cvs)
configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PARTICLEGUN_Dipion_e1_300_eta25_cff.py,v $'),
    annotation = cms.untracked.string('di-pions of 1-300 GeV randomly distributed in the Tracker acceptance')
) 
ProductionFilterSequence = cms.Sequence(generator)


