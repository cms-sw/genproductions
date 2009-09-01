# Auto generated configuration file
# using: 
# Revision: 1.123 
# Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/PyReleaseValidation/python/ConfigBuilder.py,v 
# with command line options: Tkcosmics_cfi.py -s GEN:ProductionFilterSequence,SIM,DIGI,L1,DIGI2RAW,HLT -n 1000 --conditions FrontierConditions_GlobalTag,STARTUP31X_V3::All --datatier GEN-SIM-DIGI-RAW --eventcontent FEVTHLTALL --scenario cosmics --no_exec --python_filename TKCosmics_MC_31X_STARTUP.py
#slightly modified
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
process.load('Configuration/StandardSequences/DigiCosmics_cff')
process.load('Configuration/StandardSequences/SimL1Emulator_cff')
process.load('Configuration/StandardSequences/DigiToRaw_cff')
process.load('HLTrigger/Configuration/HLT_8E29_cff')
process.load('Configuration/StandardSequences/EndOfProcess_cff')
process.load('Configuration/StandardSequences/FrontierConditions_GlobalTag_cff')
process.load('Configuration/EventContent/EventContentCosmics_cff')

process.configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.123 $'),
    annotation = cms.untracked.string('Tkcosmics_cfi.py nevts:1000'),
    name = cms.untracked.string('Simulation of cosmic muons in Tracker with B field ON')
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
    outputCommands = process.FEVTHLTALLEventContent.outputCommands,
    fileName = cms.untracked.string('Tkcosmics_cfi_py_GEN_SIM_DIGI_L1_DIGI2RAW_HLT.root'),
    dataset = cms.untracked.PSet(
        dataTier = cms.untracked.string('GEN-SIM-DIGI-RAW'),
        filterName = cms.untracked.string('')
    ),
    SelectEvents = cms.untracked.PSet(
        SelectEvents = cms.vstring('generation_step')
    )
)

# Additional output definition

# Other statements
process.GlobalTag.globaltag = 'STARTUP31X_V3::All'
process.cosmicInTracker = cms.EDFilter("CosmicGenFilterHelix",
    maxZ = cms.double(212.0),
    src = cms.InputTag("generator"),
    minPt = cms.double(0.0),
    charges = cms.vint32(1, -1),
    pdgIds = cms.vint32(-13, 13),
    minZ = cms.double(-212.0),
    radius = cms.double(80.0),
    doMonitor = cms.untracked.bool(False),
    minP = cms.double(0.0),
    propagator = cms.string('SteppingHelixPropagatorAlong')
)
process.generator = cms.EDProducer("CosMuoGenProducer",
    RadiusOfTarget = cms.double(8000.0),
    TIFOnly_constant = cms.bool(False),
    TIFOnly_linear = cms.bool(False),
    TrackerOnly = cms.bool(False),
    Verbosity = cms.bool(False),
    MaxT0 = cms.double(12.5),
    MinEnu = cms.double(10.0),
    MaxEnu = cms.double(10000.0),
    ZDistOfTarget = cms.double(15000.0),
    PlugVx = cms.double(0.0),
    MinP = cms.double(4.0),
    PlugVz = cms.double(-14000.0),
    MaxPhi = cms.double(360.0),
    MTCCHalf = cms.bool(False),
    ElossScaleFactor = cms.double(1.0),
    MinTheta = cms.double(0.0),
    MaxP = cms.double(3000.0),
    MaxTheta = cms.double(75.0),
    MinPhi = cms.double(0.0),
    MinP_CMS = cms.double(-1.0),
    MinT0 = cms.double(-12.5)
)

#Digitization cosmics
process.simSiPixelDigis.ThresholdInElectrons_FPix = 2500
process.simSiPixelDigis.ThresholdInElectrons_BPix = 2500
process.simSiPixelDigis.ThresholdSmearing_FPix = 275
process.simSiPixelDigis.ThresholdSmearing_BPix = 225
process.simSiPixelDigis.TofLowerCut = 18.5
process.simSiPixelDigis.TofUpperCut = 43.5
process.simSiStripDigis.CosmicDelayShift = 31.

#Filter
process.ProductionFilterSequence = cms.Sequence(process.generator*process.cosmicInTracker)

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
