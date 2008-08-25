# Auto generated configuration file
# using: 
# Revision: 1.57 
# Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/PyReleaseValidation/python/ConfigBuilder.py,v 
import FWCore.ParameterSet.Config as cms

process = cms.Process('GEN')

# import of standard configurations
process.load('Configuration/StandardSequences/Services_cff')
process.load('FWCore/MessageService/MessageLogger_cfi')
process.load('Configuration/StandardSequences/Generator_cff')
process.load('Configuration/StandardSequences/MixingNoPileUp_cff')
process.load('Configuration/StandardSequences/GeometryPilot2_cff')
process.load('Configuration/StandardSequences/MagneticField_cff')
process.load('Configuration/StandardSequences/Generator_cff')
process.load('Configuration/StandardSequences/VtxSmearedEarly10TeVCollision_cff')
process.load('Configuration/StandardSequences/FrontierConditions_GlobalTag_cff')
process.load('Configuration/EventContent/EventContent_cff')

process.configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: $'),
    name = cms.untracked.string('$Source: $'),
    annotation = cms.untracked.string('POMWIG SD Plus Bplus to JPsi Kplus at 10TeV')
)
process.maxEvents = cms.untracked.PSet(
    input = cms.untracked.int32(2000)
)
process.options = cms.untracked.PSet(
    Rethrow = cms.untracked.vstring('ProductNotFound')
)
# Input source
process.source = cms.Source("PomwigSource",
    HerwigParameters = cms.PSet(
        parameterSets = cms.vstring('SD1Bplus2JpsiKplus'),
        SD1Bplus2JpsiKplus = cms.vstring('NSTRU      = 14         ! H1 Pomeron Fit B', 
            'Q2WWMN     = 1E-6       ! Minimum |t|', 
            'Q2WWMX     = 4.0        ! Maximum |t|', 
            'YWWMIN     = 1E-6       ! Minimum xi', 
            'YWWMAX     = 0.2        ! Maximum xi', 
            'IPROC      = 11705      ! Process PomP -> bb', 
            'MODPDF(1)  = -1         ! Set MODPDF', 
            'MODPDF(2)  = 10150      ! Set MODPDF CTEQ61')
    ),
    herwigLhapdfVerbosity = cms.untracked.int32(0),
    ForcedDecaysParameters = cms.PSet(
        Ietmp = cms.untracked.vint32(0),
        Ibtmp = cms.untracked.vint32(321),
        Iatmp = cms.untracked.vint32(443),
        Brtmp = cms.untracked.vdouble(1.0),
        Idtmp = cms.untracked.vint32(0),
        Idktmp = cms.untracked.vint32(521),
        Imetmp = cms.untracked.vint32(0),
        Ictmp = cms.untracked.vint32(0)
    ),
    herwigHepMCVerbosity = cms.untracked.bool(False),
    h1fit = cms.int32(2),
    filterEfficiency = cms.untracked.double(1.0),
    herwigVerbosity = cms.untracked.int32(1),
    comEnergy = cms.untracked.double(10000.0),
    enableForcedDecay = cms.untracked.bool(True),
    printCards = cms.untracked.bool(True),
    crossSection = cms.untracked.double(-1.0),
    maxEventsToPrint = cms.untracked.int32(2),
    diffTopology = cms.int32(1)
)

process.genParticles.abortOnUnknownPDGCode = False

# Output definition
process.output = cms.OutputModule("PoolOutputModule",
    outputCommands = process.RAWSIMEventContent.outputCommands,
    fileName = cms.untracked.string('POMWIG_SingleDiffractivePlusBplus2JpsiKplus_10TeV_cff_py_GEN.root'),
    dataset = cms.untracked.PSet(
        dataTier = cms.untracked.string('GEN-SIM-RAW'),
        filterName = cms.untracked.string('')
    ),
    SelectEvents = cms.untracked.PSet(
        SelectEvents = cms.vstring('generation_step')
    )
)

# Other statements
process.GlobalTag.globaltag = 'IDEAL_V6::All'
process.jpsifilter = cms.EDFilter("MCSingleParticleFilter",
    MaxEta = cms.untracked.vdouble(15.0),
    MinEta = cms.untracked.vdouble(-15.0),
    MinPt = cms.untracked.vdouble(0.0),
    ParticleID = cms.untracked.vint32(443)
)
process.BpFilter = cms.EDFilter("PythiaFilter",
    ParticleID = cms.untracked.int32(521)
)
process.pomwigfilter = cms.EDFilter("PomwigFilter")
process.MuMuFilter = cms.EDFilter("MCParticlePairFilter",
    Status = cms.untracked.vint32(1, 1),
    MinPt = cms.untracked.vdouble(2.5, 2.5),
    MaxEta = cms.untracked.vdouble(2.5, 2.5),
    MinEta = cms.untracked.vdouble(-2.5, -2.5),
    ParticleCharge = cms.untracked.int32(-1),
    MaxInvMass = cms.untracked.double(3.2),
    MinInvMass = cms.untracked.double(3.0),
    ParticleID1 = cms.untracked.vint32(13),
    ParticleID2 = cms.untracked.vint32(13)
)
process.ProductionFilterSequence = cms.Sequence(process.pomwigfilter*process.BpFilter*process.jpsifilter*process.MuMuFilter)

# Path and EndPath definitions
process.generation_step = cms.Path(process.ProductionFilterSequence*process.pgen)
process.out_step = cms.EndPath(process.output)

# Schedule definition
process.schedule = cms.Schedule(process.generation_step,process.out_step)
