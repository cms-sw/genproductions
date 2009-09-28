# Auto generated configuration file
# using: 
# Revision: 1.123 
# Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/PyReleaseValidation/python/ConfigBuilder.py,v 
# with command line options: Configuration/Generator/BeamHalo_cfi.py -s GEN:ProductionFilterSequence,SIM,DIGI,L1,DIGI2RAW,HLT --eventcontent FEVTHLTALL --datatier GEN-SIM-RAW --conditions FrontierConditions_GlobalTag,STARTUP31X_V4::All --scenario nocoll -n 1000 --no_exec
# Modifications:
# EG_MAX = cms.untracked.double(7000.0) to 3500.0 (LHC beam energy) 
import FWCore.ParameterSet.Config as cms

process = cms.Process('HLT')

# import of standard configurations
process.load('Configuration/StandardSequences/Services_cff')
process.load('FWCore/MessageService/MessageLogger_cfi')
process.load('Configuration/StandardSequences/MixingNoPileUp_cff')
process.load('Configuration/StandardSequences/GeometryIdeal_cff')
process.load('Configuration/StandardSequences/MagneticField_38T_cff')
process.load('Configuration/StandardSequences/Generator_cff')
process.load('Configuration/StandardSequences/VtxSmearedNoSmear_cff')
process.load('Configuration/StandardSequences/SimNOBEAM_cff')
process.load('Configuration/StandardSequences/Digi_cff')
process.load('Configuration/StandardSequences/SimL1Emulator_cff')
process.load('Configuration/StandardSequences/DigiToRaw_cff')
process.load('HLTrigger/Configuration/HLT_8E29_cff')
process.load('Configuration/StandardSequences/EndOfProcess_cff')
process.load('Configuration/StandardSequences/FrontierConditions_GlobalTag_cff')
process.load('Configuration/EventContent/EventContent_cff')

process.configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.123 $'),
    annotation = cms.untracked.string('Configuration/Generator/BeamHalo_cfi.py nevts:1000'),
    name = cms.untracked.string('TKDPG BeamHalo Production (BSC Selection)')
)
process.maxEvents = cms.untracked.PSet(
    input = cms.untracked.int32(1000)
)
process.options = cms.untracked.PSet(
    Rethrow = cms.untracked.vstring('ProductNotFound')
)
# Input source
process.source = cms.Source("EmptySource")

# Output definition
process.output = cms.OutputModule("PoolOutputModule",
    splitLevel = cms.untracked.int32(0),
    outputCommands = process.FEVTHLTALLEventContent.outputCommands,
    fileName = cms.untracked.string('BeamHalo_cfi_py_GEN_SIM_DIGI_L1_DIGI2RAW_HLT.root'),
    dataset = cms.untracked.PSet(
        dataTier = cms.untracked.string('GEN-SIM-RAW'),
        filterName = cms.untracked.string('')
    ),
    SelectEvents = cms.untracked.PSet(
        SelectEvents = cms.vstring('HLT_BackwardBSC','HLT_ForwardBSC')
    )
)

# Additional output definition

# Other statements
process.GlobalTag.globaltag = 'STARTUP31X_V4::All'
process.generator = cms.EDProducer("BeamHaloProducer",
    LHC_B2 = cms.untracked.int32(1),
    EG_MIN = cms.untracked.double(10.0),
    EG_MAX = cms.untracked.double(3500.0),
    shift_bx = cms.untracked.int32(0),
    IW_MUO = cms.untracked.int32(1),
    BXNS = cms.untracked.double(25.0),
    GENMOD = cms.untracked.int32(1),
    LHC_B1 = cms.untracked.int32(1),
    IW_HAD = cms.untracked.int32(0)
)
process.ProductionFilterSequence = cms.Sequence(process.generator)

# Path and EndPath definitions
process.generation_step = cms.Path(process.pgen)
process.simulation_step = cms.Path(process.psim)
process.digitisation_step = cms.Path(process.pdigi)
process.L1simulation_step = cms.Path(process.SimL1Emulator)
process.digi2raw_step = cms.Path(process.DigiToRaw)
process.endjob_step = cms.Path(process.endOfProcess)
process.out_step = cms.EndPath(process.output)

# Schedule definition
process.schedule = cms.Schedule(process.generation_step,process.simulation_step,process.digitisation_step,process.L1simulation_step,process.digi2raw_step)
process.schedule.extend(process.HLTSchedule)
process.schedule.extend([process.endjob_step,process.out_step])
# special treatment in case of production filter sequence  
for path in process.paths: 
    getattr(process,path)._seq = process.ProductionFilterSequence*getattr(process,path)._seq
