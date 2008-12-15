# Auto generated configuration file
# using: 
# Revision: 1.77 
# Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/PyReleaseValidation/python/ConfigBuilder.py,v 
# with command line options: Configuration/GenProduction/python/HerwigJimmy_QCD_Pt_800.py -s GEN:ProductionFilterSequence --eventcontent RAWSIM --datatier GEN --conditions FrontierConditions_GlobalTag,IDEAL_V9::All --no_exec -n 1000
# Add process.genParticles.abortOnUnknownPDGCode = False
import FWCore.ParameterSet.Config as cms

process = cms.Process('GEN')

# import of standard configurations
process.load('Configuration/StandardSequences/Services_cff')
process.load('FWCore/MessageService/MessageLogger_cfi')
process.load('Configuration/StandardSequences/MixingNoPileUp_cff')
process.load('Configuration/StandardSequences/GeometryPilot2_cff')
process.load('Configuration/StandardSequences/MagneticField_38T_cff')
process.load('Configuration/StandardSequences/Generator_cff')
process.load('Configuration/StandardSequences/VtxSmearedEarly10TeVCollision_cff')
process.load('Configuration/StandardSequences/FrontierConditions_GlobalTag_cff')
process.load('Configuration/EventContent/EventContent_cff')

process.configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.77 $'),
    annotation = cms.untracked.string('Configuration/GenProduction/python/HerwigJimmy_QCD_Pt_800.py nevts:1000'),
    name = cms.untracked.string('PyReleaseValidation')
)
process.maxEvents = cms.untracked.PSet(
    input = cms.untracked.int32(1000)
)
process.options = cms.untracked.PSet(
    Rethrow = cms.untracked.vstring('ProductNotFound')
)
# Input source
process.source = cms.Source("Herwig6Source",
    HerwigParameters = cms.PSet(
        herwigQCDjets = cms.vstring('IPROC      = 1500       ! QCD 2->2 processes', 
            'PTMIN      = 800.       ! minimum pt in hadronic jet', 
            'MODPDF(1)  = 10041      ! PDF set according to LHAGLUE', 
            'MODPDF(2)  = 10041      ! CTEQ6L', 
            'JMUEO      = 1          ! multiparton interaction model', 
            'PTJIM      = 4.449      ! 2.8x(sqrt(s)/1.8TeV)^0.27 @ 10 TeV', 
            'JMRAD(73)  = 1.8        ! inverse proton radius squared', 
            'PRSOF      = 0.0        ! prob. of a soft underlying event', 
            'MAXER      = 1000000    ! max error'),
        parameterSets = cms.vstring('herwigQCDjets')
    ),
    doMPInteraction = cms.untracked.bool(True),
    useJimmy = cms.untracked.bool(True),
    herwigHepMCVerbosity = cms.untracked.bool(False),
    filterEfficiency = cms.untracked.double(1.0),
    herwigVerbosity = cms.untracked.int32(0),
    comEnergy = cms.untracked.double(10000.0),
    lhapdfSetPath = cms.untracked.string('/afs/cern.ch/sw/lcg/external/MCGenerators/lhapdf/5.2.3/share/PDFsets'),
    printCards = cms.untracked.bool(False),
    crossSection = cms.untracked.double(10.88),
    maxEventsToPrint = cms.untracked.int32(0)
)
process.genParticles.abortOnUnknownPDGCode = False

# Output definition
process.output = cms.OutputModule("PoolOutputModule",
    outputCommands = process.RAWSIMEventContent.outputCommands,
    fileName = cms.untracked.string('HerwigJimmy_QCD_Pt_800_py_GEN.root'),
    dataset = cms.untracked.PSet(
        dataTier = cms.untracked.string('GEN'),
        filterName = cms.untracked.string('')
    ),
    SelectEvents = cms.untracked.PSet(
        SelectEvents = cms.vstring('generation_step')
    )
)

# Additional output definition

# Other statements
process.GlobalTag.globaltag = 'IDEAL_V9::All'
process.emptyEventFilter = cms.EDFilter("Herwig6Filter")
process.ProductionFilterSequence = cms.Sequence(process.emptyEventFilter)

# Path and EndPath definitions
process.generation_step = cms.Path(process.ProductionFilterSequence*process.pgen)
process.out_step = cms.EndPath(process.output)

# Schedule definition
process.schedule = cms.Schedule(process.generation_step,process.out_step)
