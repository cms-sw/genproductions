# Auto generated configuration file
# using:
# $Revision: 1.2 $
# $Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/MadGraph_Unmatched_XQCUT15_GEN_10TeV_cff_py_GEN_IDEAL.py,v $
import FWCore.ParameterSet.Config as cms

process = cms.Process('GEN')

# import of standard configurations
process.load('Configuration/StandardSequences/Services_cff')
process.load('FWCore/MessageService/MessageLogger_cfi')
process.load('Configuration/StandardSequences/Generator_cff')
process.load('Configuration/StandardSequences/MixingNoPileUp_cff')
process.load('Configuration/StandardSequences/GeometryPilot2_cff')
process.load('Configuration/StandardSequences/MagneticField_38T_cff')
process.load('Configuration/StandardSequences/Generator_cff')
process.load('Configuration/StandardSequences/VtxSmearedEarly10TeVCollision_cff')
process.load('Configuration/StandardSequences/FrontierConditions_GlobalTag_cff')
process.load('Configuration/EventContent/EventContent_cff')

process.ReleaseValidation = cms.untracked.PSet(
    primaryDatasetName = cms.untracked.string('RelValConfiguration/GenProduction/python/MadGraph_Unmatched_XQCUT15_GEN_10TeV_cff_py'),
    totalNumberOfEvents = cms.untracked.int32(5000),
    eventsPerJob = cms.untracked.int32(250)
)
process.configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string(''),
    annotation = cms.untracked.string('Unmatched MadGraph-PYTHIA6 at 10TeV, qcut 15'),
    name = cms.untracked.string('')
)
process.maxEvents = cms.untracked.PSet(
    input = cms.untracked.int32(1000)
)
process.options = cms.untracked.PSet(
    Rethrow = cms.untracked.vstring('ProductNotFound')
)
# Input source
process.source = cms.Source("MadGraphSource",
    produceEventTreeFile = cms.untracked.bool(False),
    MEMAIN_iexcfile = cms.untracked.uint32(0),
    fileNames = cms.untracked.vstring('file:events.lhe'),
    MEMAIN_qcut = cms.untracked.double(15.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    MEMAIN_etaclmax = cms.untracked.double(5.0),
    firstEvent = cms.untracked.uint32(0),
    minimalLH = cms.untracked.bool(False),
    pythiaPylistVerbosity = cms.untracked.int32(0),
    getInputFromMCDB = cms.untracked.bool(False),
    maxEventsToPrint = cms.untracked.int32(5),
    MCDBArticleID = cms.int32(0),
    PythiaParameters = cms.PSet(
        pythiaUESettings = cms.vstring('MSTJ(11)=3     ! Choice of the fragmentation function',
            'MSTJ(22)=2     ! Decay those unstable particles',
            'PARJ(71)=10 .  ! for which ctau  10 mm',
            'MSTP(2)=1      ! which order running alphaS',
            'MSTP(33)=0     ! no K factors in hard cross sections',
            'MSTP(51)=10042     ! CTEQ6L1 structure function chosen',
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
            'PARP(62)=1.25   ! ',
            'PARP(64)=0.2    ! ',
            'MSTP(91)=1     !',
            'PARP(91)=2.1   ! kt distribution',
            'PARP(93)=15.0  ! '),
        pythiaCMSDefaults = cms.vstring('PMAS(5,1)=4.4 ! b quarks mass',
            'PMAS(6,1)=172.4 ! t quarks mass',
            'MSTJ(1)=1 !...Fragmentation/hadronization on or off',
            'MSTP(61)=1 ! Parton showering on or off',
            'MSTP(143)=0 ! MUST BE 1 FOR THE MATCHING ROUTINE TO RUN!!!!',
            'MSEL=0 ! User defined processes/Full user control'),
        parameterSets = cms.vstring('pythiaUESettings',
            'pythiaCMSDefaults')
    )
)

# Output definition
process.output = cms.OutputModule("PoolOutputModule",
    outputCommands = process.RAWSIMEventContent.outputCommands,
    fileName = cms.untracked.string('MadGraph_Unmatched_XQCUT15_GEN_10TeV_cff_py_GEN.root'),
    dataset = cms.untracked.PSet(
        dataTier = cms.untracked.string('GEN'),
        filterName = cms.untracked.string('IDEAL_V6')
    ),
    SelectEvents = cms.untracked.PSet(
        SelectEvents = cms.vstring('generation_step')
    )
)

# Other statements
process.GlobalTag.globaltag = 'IDEAL_V6::All'

# Path and EndPath definitions
process.generation_step = cms.Path(process.pgen)
process.out_step = cms.EndPath(process.output)

# Schedule definition
process.schedule = cms.Schedule(process.generation_step,process.out_step)
