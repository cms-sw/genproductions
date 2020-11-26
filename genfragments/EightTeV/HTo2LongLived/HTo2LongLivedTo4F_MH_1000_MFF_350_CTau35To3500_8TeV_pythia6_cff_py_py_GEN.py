# Auto generated configuration file
# using: 
# Revision: 1.352 
# Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/PyReleaseValidation/python/ConfigBuilder.py,v 
# with command line options: Configuration/GenProduction/python/EightTeV/HTo2LongLivedTo4F_MH_1000_MFF_350_CTau35To3500_8TeV_pythia6_cff.py.py -s GEN --conditions START50_V13::All --beamspot Realistic8TeVCollision --datatier GEN-SIM --eventcontent RAWDEBUG -n 1000 --no_exec
import FWCore.ParameterSet.Config as cms

process = cms.Process('GEN')

# import of standard configurations
process.load('Configuration.StandardSequences.Services_cff')
process.load('SimGeneral.HepPDTESSource.pythiapdt_cfi')
process.load('FWCore.MessageService.MessageLogger_cfi')
process.load('Configuration.EventContent.EventContent_cff')
process.load('SimGeneral.MixingModule.mixNoPU_cfi')
process.load('Configuration.StandardSequences.GeometryDB_cff')
process.load('Configuration.StandardSequences.MagneticField_38T_cff')
process.load('Configuration.StandardSequences.Generator_cff')
process.load('IOMC.EventVertexGenerators.VtxSmearedRealistic8TeVCollision_cfi')
process.load('GeneratorInterface.Core.genFilterSummary_cff')
process.load('Configuration.StandardSequences.EndOfProcess_cff')
process.load('Configuration.StandardSequences.FrontierConditions_GlobalTag_cff')

process.maxEvents = cms.untracked.PSet(
    input = cms.untracked.int32(1000)
)

# Input source
process.source = cms.Source("EmptySource")

process.options = cms.untracked.PSet(

)

# Production Info
process.configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.352 $'),
    annotation = cms.untracked.string('Configuration/GenProduction/python/EightTeV/HTo2LongLivedTo4F_MH_1000_MFF_350_CTau35To3500_8TeV_pythia6_cff.py.py nevts:1000'),
    name = cms.untracked.string('PyReleaseValidation')
)

# Output definition

process.RAWDEBUGoutput = cms.OutputModule("PoolOutputModule",
    splitLevel = cms.untracked.int32(0),
    eventAutoFlushCompressedSize = cms.untracked.int32(5242880),
    outputCommands = process.RAWDEBUGEventContent.outputCommands,
    fileName = cms.untracked.string('HTo2LongLivedTo4F_MH_1000_MFF_350_CTau35To3500_8TeV_pythia6_cff_py_py_GEN.root'),
    dataset = cms.untracked.PSet(
        filterName = cms.untracked.string(''),
        dataTier = cms.untracked.string('GEN-SIM')
    ),
    SelectEvents = cms.untracked.PSet(
        SelectEvents = cms.vstring('generation_step')
    )
)

# Additional output definition

# Other statements
process.GlobalTag.globaltag = 'START50_V13::All'

process.genParticles = cms.EDProducer("GenParticleProducer",
    saveBarCodes = cms.untracked.bool(True),
    src = cms.InputTag("generator"),
    abortOnUnknownPDGCode = cms.untracked.bool(False)
)


process.genParticlesForFilter = cms.EDProducer("GenParticleProducer",
    saveBarCodes = cms.untracked.bool(True),
    src = cms.InputTag("generator"),
    abortOnUnknownPDGCode = cms.untracked.bool(False)
)


process.XtoFFbarFilter = cms.EDFilter("XtoFFbarFilter",
    src = cms.InputTag("genParticlesForFilter"),
    idMotherY = cms.vint32(6000111, 6000112, 6000113, 6000114, 6001111, 
        6001112, 6001113, 6001114, 6002111, 6002112, 
        6002113, 6002114, 6003111, 6003112, 6003113, 
        6003114),
    idMotherX = cms.vint32(6000111, 6000112, 6000113, 6000114, 6001111, 
        6001112, 6001113, 6001114, 6002111, 6002112, 
        6002113, 6002114, 6003111, 6003112, 6003113, 
        6003114),
    idDaughterF = cms.vint32(1, 2, 3, 4, 5, 
        6, 11, 13, 15),
    idDaughterG = cms.vint32(1, 2, 3, 4, 5, 
        6, 11, 13, 15)
)


process.generator = cms.EDFilter("Pythia6GeneratorFilter",
    maxEventsToPrint = cms.untracked.int32(10),
    pythiaPylistVerbosity = cms.untracked.int32(3),
    filterEfficiency = cms.untracked.double(0.99),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(8000),
    crossSection = cms.untracked.double(-1.0),
    UseExternalGenerators = cms.untracked.bool(False),
    PythiaParameters = cms.PSet(
        pythiaUESettings = cms.vstring('MSTU(21)=1     ! Check on possible errors during program execution', 
            'MSTJ(22)=2     ! Decay those unstable particles', 
            'PARJ(71)=10 .  ! for which ctau  10 mm', 
            'MSTP(33)=0     ! no K factors in hard cross sections', 
            'MSTP(2)=1      ! which order running alphaS', 
            'MSTP(51)=10042 ! structure function chosen (external PDF CTEQ6L1)', 
            'MSTP(52)=2     ! work with LHAPDF', 
            'PARP(82)=1.921 ! pt cutoff for multiparton interactions', 
            'PARP(89)=1800. ! sqrts for which PARP82 is set', 
            'PARP(90)=0.227 ! Multiple interactions: rescaling power', 
            'MSTP(95)=6     ! CR (color reconnection parameters)', 
            'PARP(77)=1.016 ! CR', 
            'PARP(78)=0.538 ! CR', 
            'PARP(80)=0.1   ! Prob. colored parton from BBR', 
            'PARP(83)=0.356 ! Multiple interactions: matter distribution parameter', 
            'PARP(84)=0.651 ! Multiple interactions: matter distribution parameter', 
            'PARP(62)=1.025 ! ISR cutoff', 
            'MSTP(91)=1     ! Gaussian primordial kT', 
            'PARP(93)=10.0  ! primordial kT-max', 
            'MSTP(81)=21    ! multiple parton interactions 1 is Pythia default', 
            'MSTP(82)=4     ! Defines the multi-parton model'),
        parameterSets = cms.vstring('pythiaUESettings', 
            'pythiaParameters', 
            'PYUPDAParameters'),
        PYUPDAParameters = cms.vstring("PYUPDAFILE = \'dummy\'", 
            "PYUPDAFILE = \'Configuration/Generator/data/HTo2LongLivedTo4F_MH_1000_MFF_350_CTau35To3500_pythia6.pyupda\'"),
        pythiaParameters = cms.vstring('MSTJ(22)=1    ! Decay ALL unstable particles', 
            'MSTP(95)=0', 
            'MSEL=0', 
            'MSUB(152)=1', 
            'MWID(35)=2 ! Let me set H0 propertiesPMAS(35,2)=1.', 
            'PMAS(35,1)=1000 ! Set Higgs mass. This probably has no effect ...')
    )
)


process.ProductionFilterSequence = cms.Sequence(process.generator+process.genParticlesForFilter+process.XtoFFbarFilter)

# Path and EndPath definitions
process.generation_step = cms.Path(process.pgen)
process.genfiltersummary_step = cms.EndPath(process.genFilterSummary)
process.endjob_step = cms.EndPath(process.endOfProcess)
process.RAWDEBUGoutput_step = cms.EndPath(process.RAWDEBUGoutput)

# Schedule definition
process.schedule = cms.Schedule(process.generation_step,process.genfiltersummary_step,process.endjob_step,process.RAWDEBUGoutput_step)
# filter all path with the production filter sequence
for path in process.paths:
	getattr(process,path)._seq = process.ProductionFilterSequence * getattr(process,path)._seq 

