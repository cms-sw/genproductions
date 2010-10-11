import FWCore.ParameterSet.Config as cms

from Configuration.Generator.PyquenDefaultSettings_cff import *
hiSignal = cms.EDProducer("Pythia6PtYDistGun",
    PGunParameters = cms.PSet(
        ParticleID = cms.vint32(553),
        kinematicsFile = cms.FileInPath('GeneratorInterface/HiGenCommon/data/flatYPt.root'),
        PtBinning = cms.int32(100000),
        YBinning = cms.int32(500),
        MaxPt = cms.double(20.0),
        MinPt = cms.double(0.0),
        MaxY = cms.double(2.5),
        MinY = cms.double(-2.5),
        MinPhi = cms.double(-3.1415926535897932384),
        MaxPhi = cms.double(3.1415926535897932384)
    ),
    PythiaParameters = cms.PSet(
        pyquenPythiaDefaultBlock,
        parameterSets = cms.vstring(
            'pythiaUESettings',
            'pythiaUpsilonToMuons'
        )
    )
)

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.2 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/SingleUpsilonmumu_FlatPt0to20_cfi.py,v $'),
    annotation = cms.untracked.string('Pythia Gun Y -> mu mu')
    )

ProductionFilterSequence = cms.Sequence(hiSignal)
