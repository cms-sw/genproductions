# Auto generated configuration file
# using: 
# Revision: 1.77.2.1 
# Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/PyReleaseValidation/python/ConfigBuilder.py,v 
# with command line options: Configuration/GenProduction/python/ALPGEN_bb_4jet_10TeV_cff.py -s GEN:ProductionFilterSequence --eventcontent RAWSIM --datatier GEN --conditions FrontierConditions_GlobalTag,IDEAL_V9::All -n 10 --no_exec
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
    version = cms.untracked.string('$Revision: 1.2 $'),
    annotation = cms.untracked.string('Default ALPGEN_bb_4jet_cff.py'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/UserCode/sergei/final_cfg/ALPGEN_bb_4jet_10TeV_cff_py_GEN_IDEAL.py,v $')
)
process.maxEvents = cms.untracked.PSet(
    input = cms.untracked.int32(10)
)
process.options = cms.untracked.PSet(
    Rethrow = cms.untracked.vstring('ProductNotFound')
)
# Input source
process.source = cms.Source("AlpgenSource",
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    fileNames = cms.untracked.vstring('file:bbj4'),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.untracked.double(10000.0),
    GeneratorParameters = cms.PSet(
        parameterSets = cms.vstring('generator'),
        generator = cms.vstring('IXpar(2) =  0           ! in/ex-clusive: 0/1', 
            'RXpar(1) = 20.          ! minimum cluster Et', 
            'RXpar(2) =  0.7         ! minimum cluster dR')
    ),
    crossSection = cms.untracked.double(13979.0),
    maxEventsToPrint = cms.untracked.int32(1),
    PythiaParameters = cms.PSet(
        parameterSets = cms.vstring('pythia'),
        pythia = cms.vstring('MSEL=0              !(D=1) ',
            'MSTJ(11)=3          !Choice of the fragmentation function',
            'MSTP(143)=1         !Call the matching routine in ALPGEN',
            'MSTJ(22)=2     ! Decay those unstable particles',
            'PARJ(71)=10 .  ! for which ctau  10 mm',
            'MSTP(2)=1      ! which order running alphaS',
            'MSTP(33)=0     ! no K factors in hard cross sections',
            'MSTP(51)=10042 ! CTEQ6L1 structure function chosen',
            'MSTP(52)=2     ! work with LHAPDF',
            'MSTP(81)=1     ! multiple parton interactions 1 is Pythia default',
            'MSTP(82)=4     ! Defines the multi-parton model',
            'MSTU(21)=1     ! Check on possible errors during program execution',
            'PARP(82)=1.8387   ! pt cutoff for multiparton interactions',
            'PARP(89)=1960. ! sqrts for which PARP82 is set',
            'PARP(83)=0.5   ! Multiple interactions: matter distrbn parameter',
            'PARP(84)=0.4   ! Multiple interactions: matter distribution parameter',
            'PARP(90)=0.16  ! Multiple interactions: rescaling power',
            'PARP(67)=2.5    ! amount of initial-state radiation',
            'PARP(85)=1.0  ! gluon prod. mechanism in MI',
            'PARP(86)=1.0  ! gluon prod. mechanism in MI',
            'PARP(62)=1.25  ! ',
            'PARP(64)=0.2   ! ',
            'MSTP(91)=1     ! ',
            'PARP(91)=2.1   ! kt distribution',
            'PARP(93)=15.0  ! ')
    )
)

process.AlpgenEventContent = cms.PSet(
  outputCommands = cms.untracked.vstring(
    'keep edmAlpgenInfoProduct_source_*_*',
    'keep edmAlpWgtFileInfoProduct_source_*_*'
  )
)
process.RAWSIMEventContent.outputCommands.extend(process.AlpgenEventContent.outputCommands)

# Output definition
process.output = cms.OutputModule("PoolOutputModule",
    outputCommands = process.RAWSIMEventContent.outputCommands,
    fileName = cms.untracked.string('ALPGEN_bb_4jet_10TeV_cff_py_GEN.root'),
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
process.filter = cms.EDFilter("AlpgenEmptyEventFilter")
process.ProductionFilterSequence = cms.Sequence(process.filter)

# Path and EndPath definitions
process.generation_step = cms.Path(process.ProductionFilterSequence*process.pgen)
process.out_step = cms.EndPath(process.output)

# Schedule definition
process.schedule = cms.Schedule(process.generation_step,process.out_step)
