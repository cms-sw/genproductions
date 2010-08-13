# Auto generated configuration file
# using: 
# Revision: 1.168 
# Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/PyReleaseValidation/python/ConfigBuilder.py,v 
# with command line options: Tkcosmics_cfi.py -s DIGI,L1,DIGI2RAW -n -1 --conditions FrontierConditions_GlobalTag,COSMC3X_V1::All --datatier GEN-SIM-DIGI-RAW --eventcontent FEVTHLTALL --scenario cosmics --magField 0T --no_exec --python_filename TKDPG_Cosmics_MC_35X_STARTUP_0T.py
#slighlty modified: SiStrip+pixel digitizer - peak mode
import FWCore.ParameterSet.Config as cms

process = cms.Process('DIGI2RAW')

# import of standard configurations
process.load('Configuration.StandardSequences.Services_cff')
process.load('SimGeneral.HepPDTESSource.pythiapdt_cfi')
process.load('FWCore.MessageService.MessageLogger_cfi')
process.load('Configuration.StandardSequences.MixingNoPileUp_cff')
process.load('Configuration.StandardSequences.GeometryExtended_cff')
process.load('Configuration.StandardSequences.MagneticField_0T_cff')
process.load('Configuration.StandardSequences.DigiCosmics_cff')
process.load('Configuration.StandardSequences.SimL1Emulator_cff')
process.load('Configuration.StandardSequences.DigiToRaw_cff')
process.load('Configuration.StandardSequences.EndOfProcess_cff')
process.load('Configuration.StandardSequences.FrontierConditions_GlobalTag_cff')
process.load('Configuration.EventContent.EventContentCosmics_cff')

process.configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.168 $'),
    annotation = cms.untracked.string('Tkcosmics_cfi.py nevts:-1'),
    name = cms.untracked.string('Cosmic muons in Tracker with B field ON')
)
process.maxEvents = cms.untracked.PSet(
    input = cms.untracked.int32(-1)
)
process.options = cms.untracked.PSet(

)
# Input source
process.source = cms.Source("PoolSource",
    fileNames = cms.untracked.vstring('Tkcosmics_cfi.py_SIM.root')
)

# Output definition
process.output = cms.OutputModule("PoolOutputModule",
    splitLevel = cms.untracked.int32(0),
    outputCommands = process.FEVTHLTALLEventContent.outputCommands,
    fileName = cms.untracked.string('Tkcosmics_cfi_py_DIGI_L1_DIGI2RAW.root'),
    dataset = cms.untracked.PSet(
        dataTier = cms.untracked.string('GEN-SIM-DIGI-RAW'),
        filterName = cms.untracked.string('')
    )
)

# Additional output definition

# Other statements
process.GlobalTag.globaltag = 'COSMC3X_V1::All'

#Digitizer
process.simSiPixelDigis.TofLowerCut = 18.5
process.simSiPixelDigis.TofUpperCut = 43.5
process.simSiStripDigis.CosmicDelayShift = 31.
process.simSiStripDigis.APVpeakmode = True

# Path and EndPath definitions
process.digitisation_step = cms.Path(process.pdigi)
process.L1simulation_step = cms.Path(process.SimL1Emulator)
process.digi2raw_step = cms.Path(process.DigiToRaw)
process.endjob_step = cms.Path(process.endOfProcess)
process.out_step = cms.EndPath(process.output)

# Schedule definition
process.schedule = cms.Schedule(process.digitisation_step,process.L1simulation_step,process.digi2raw_step,process.endjob_step,process.out_step)
