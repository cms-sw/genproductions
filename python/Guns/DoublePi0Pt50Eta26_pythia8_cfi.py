import FWCore.ParameterSet.Config as cms

generator = cms.EDFilter("Pythia8PtGun",
    maxEventsToPrint = cms.untracked.int32(1),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    pythiaHepMCVerbosity = cms.untracked.bool(True),
    
    PGunParameters = cms.PSet(
        MaxPt = cms.double(50.01),
        MinPt = cms.double(49.99),
        AddAntiParticle = cms.bool(True),
        ParticleID = cms.vint32(111),
        MaxEta = cms.double(2.6),
        MaxPhi = cms.double(3.14159265359),
        MinEta = cms.double(2.6),
        MinPhi = cms.double(-3.14159265359) ## in radians

    ),
    Verbosity = cms.untracked.int32(0), ## set to 1 (or greater)  for printouts
    psethack = cms.string('single pi0 pt 50'),
    firstRun = cms.untracked.uint32(1),
    PythiaParameters = cms.PSet(parameterSets = cms.vstring())
)
