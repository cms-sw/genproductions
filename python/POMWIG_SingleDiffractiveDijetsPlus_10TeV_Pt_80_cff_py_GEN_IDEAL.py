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
    version = cms.untracked.string('$Revision: 1.1 $'),
    annotation = cms.untracked.string('POMWIG SD plus Di-jets ptmin 80 GeV at 10TeV'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/POMWIG_SingleDiffractiveDijetsPlus_10TeV_Pt_80_cff_py_GEN_IDEAL.py,v $')
)
process.maxEvents = cms.untracked.PSet(
    input = cms.untracked.int32(1000)
)
process.options = cms.untracked.PSet(
    Rethrow = cms.untracked.vstring('ProductNotFound')
)
# Input source
process.source = cms.Source("PomwigSource",
    HerwigParameters = cms.PSet(
        parameterSets = cms.vstring('SD1InclusiveJets'),
        SD1InclusiveJets = cms.vstring('NSTRU      = 14         ! H1 Pomeron Fit B', 
            'Q2WWMN     = 1E-6       ! Minimum |t|', 
            'Q2WWMX     = 4.0        ! Maximum |t|', 
            'YWWMIN     = 1E-6       ! Minimum xi', 
            'YWWMAX     = 0.2        ! Maximum xi', 
            'IPROC      = 11500      ! Process PomP -> jets', 
            'PTMIN      = 80         ! 2->2 PT min', 
            'MODPDF(1)  = -1         ! Set MODPDF', 
            'MODPDF(2)  = 10150      ! Set MODPDF CTEQ61')
    ),
    herwigLhapdfVerbosity = cms.untracked.int32(0),
    herwigHepMCVerbosity = cms.untracked.bool(False),
    h1fit = cms.int32(2),
    filterEfficiency = cms.untracked.double(1.0),
    herwigVerbosity = cms.untracked.int32(1),
    comEnergy = cms.untracked.double(10000.0),
    printCards = cms.untracked.bool(True),
    crossSection = cms.untracked.double(-1.0),
    maxEventsToPrint = cms.untracked.int32(2),
    diffTopology = cms.int32(1)
)

process.genParticles.abortOnUnknownPDGCode = False

# Output definition
process.output = cms.OutputModule("PoolOutputModule",
    outputCommands = process.RAWSIMEventContent.outputCommands,
    fileName = cms.untracked.string('POMWIG_SingleDiffractiveDijetsPlus_10TeV_Pt_80_cff_py_GEN.root'),
    dataset = cms.untracked.PSet(
        dataTier = cms.untracked.string('GEN'),
        filterName = cms.untracked.string('')
    ),
    SelectEvents = cms.untracked.PSet(
        SelectEvents = cms.vstring('generation_step')
    )
)

# Other statements
process.GlobalTag.globaltag = 'IDEAL_V9::All'
process.pomwigfilter = cms.EDFilter("PomwigFilter")
process.ProductionFilterSequence = cms.Sequence(process.pomwigfilter)

# Path and EndPath definitions
process.generation_step = cms.Path(process.ProductionFilterSequence*process.pgen)
process.out_step = cms.EndPath(process.output)

# Schedule definition
process.schedule = cms.Schedule(process.generation_step,process.out_step)
