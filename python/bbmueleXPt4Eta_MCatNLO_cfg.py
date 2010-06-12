import FWCore.ParameterSet.Config as cms


#source = cms.Source("MCDBSource",
#    articleID = cms.uint32(1033),
#    supportedProtocols = cms.untracked.vstring('rfio')
#)

muelegenfilter = cms.EDFilter("MCSmartSingleParticleFilter",
    MaxDecayRadius = cms.untracked.vdouble(2000.0, 2000.0, 2000.0, 2000.0),
    Status = cms.untracked.vint32(1, 1, 1, 1),
    MinPt = cms.untracked.vdouble(4, 4, 4, 4),
    ParticleID = cms.untracked.vint32(13, -13, 11, -11),
    MaxEta = cms.untracked.vdouble(2.5, 2.5, 2.5, 2.5),
    MinEta = cms.untracked.vdouble(-2.5, -2.5, -2.5, -2.5),
    MaxDecayZ = cms.untracked.vdouble(4000.0, 4000.0, 4000.0, 4000.0),
    MinDecayZ = cms.untracked.vdouble(-4000.0, -4000.0, -4000.0, -4000.0)
)

generator = cms.EDFilter("Herwig6HadronizerFilter",
        comEnergy = cms.double(7000.0),
        useJimmy = cms.bool(False),
        doMPInteraction = cms.bool(False),

        herwigHepMCVerbosity = cms.untracked.bool(False),
        herwigVerbosity = cms.untracked.int32(1),
        printCards = cms.untracked.bool(True),
        maxEventsToPrint = cms.untracked.int32(0),

        crossSection = cms.untracked.double(-1.0),
        filterEfficiency = cms.untracked.double(1.0),

        emulatePythiaStatusCodes = cms.untracked.bool(False),

        numTrialsMPI = cms.untracked.int32(1),

        HerwigParameters = cms.PSet(
                parameterSets = cms.vstring(
                        'herwigMcatnlo'
                ),
                herwigMcatnlo = cms.vstring(
                        'PTMIN      = 0.5       ! minimum pt in hadronic jet'
                )
        )
)

ProductionFilterSequence = cms.Sequence(generator*muelegenfilter)
