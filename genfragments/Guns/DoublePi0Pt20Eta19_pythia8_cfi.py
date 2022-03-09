import FWCore.ParameterSet.Config as cms

generator = cms.EDFilter("Pythia8PtGun",
    maxEventsToPrint = cms.untracked.int32(1),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    pythiaHepMCVerbosity = cms.untracked.bool(True),
    
    PGunParameters = cms.PSet(
        MaxPt = cms.double(20.01),
        MinPt = cms.double(19.99),
        AddAntiParticle = cms.bool(True),
        ParticleID = cms.vint32(111),
        MaxEta = cms.double(1.9),
        MaxPhi = cms.double(3.14159265359),
        MinEta = cms.double(1.9),
        MinPhi = cms.double(-3.14159265359) ## in radians

    ),
    Verbosity = cms.untracked.int32(0), ## set to 1 (or greater)  for printouts
    psethack = cms.string('single pi0 pt 20'),
    firstRun = cms.untracked.uint32(1),
    PythiaParameters = cms.PSet(parameterSets = cms.vstring())
)
