# Auto generated configuration file slightly modified (event content)
# Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/PyReleaseValidation/python/ConfigBuilder.py,v 
# with command line options: Configuration/Generator/python/BeamHalo_cfi.py -s RAW2DIGI,RECO --filein file:BeamHalo_cfi_py_GEN_SIM_DIGI_L1_DIGI2RAW_HLT.root --eventcontent RecoTrackerRECO --datatier GEN-SIM-RECO --conditions FrontierConditions_GlobalTag,STARTUP_V11::All -n -1 --no_exec
import FWCore.ParameterSet.Config as cms

process = cms.Process('RECO')

# import of standard configurations
process.load('Configuration/StandardSequences/Services_cff')
process.load('FWCore/MessageService/MessageLogger_cfi')
process.load('Configuration/StandardSequences/MixingNoPileUp_cff')
process.load('Configuration/StandardSequences/GeometryPilot2_cff')
process.load('Configuration/StandardSequences/MagneticField_38T_cff')
process.load('Configuration/StandardSequences/RawToDigi_cff')
process.load('Configuration/StandardSequences/Reconstruction_cff')
process.load('Configuration/StandardSequences/EndOfProcess_cff')
process.load('Configuration/StandardSequences/FrontierConditions_GlobalTag_cff')
process.load('RecoTracker.Configuration.RecoTrackerBHM_cff')
process.load('RecoTracker/Configuration/RecoTrackerBHM_EventContent_cff')
process.myeventcontent = cms.PSet(
     outputCommands = cms.untracked.vstring(
     'keep *_trackingtruthprod_*_*', 
         'keep *_electrontruth_*_*', 
         'keep *_mergedtruth_MergedTrackTruth_*',
	 'keep *_simSiPixelDigis_*_*',
         'keep *_simSiStripDigis_*_*',
	 'keep *_siStripClusters_*_*',
	 'keep *_siPixelClusters_*_*'
	 	 	 )
 ) 
process.RecoTrackerRECO.outputCommands.extend(process.myeventcontent.outputCommands)

process.configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    annotation = cms.untracked.string('Configuration/Generator/python/BeamHalo_cfi.py nevts:-1'),
    name = cms.untracked.string('TKDPG BH Muons sample')
)
process.maxEvents = cms.untracked.PSet(
    input = cms.untracked.int32(-1)
)
process.options = cms.untracked.PSet(
    Rethrow = cms.untracked.vstring('ProductNotFound')
)
# Input source
process.source = cms.Source("PoolSource",
    fileNames = cms.untracked.vstring('file:BeamHalo_cfi_py_GEN_SIM_DIGI_L1_DIGI2RAW_HLT.root')
)

# Output definition
process.output = cms.OutputModule("PoolOutputModule",
    process.RecoTrackerRECO,
    fileName = cms.untracked.string('BeamHalo_cfi_py_RAW2DIGI_RECO.root'),
    dataset = cms.untracked.PSet(
        dataTier = cms.untracked.string('GEN-SIM-RECO'),
        filterName = cms.untracked.string('')
    )
)

# Additional output definition

# Other statements
process.GlobalTag.globaltag = 'STARTUP_V11::All'

# Path and EndPath definitions
process.raw2digi_step = cms.Path(process.RawToDigi)
process.reconstruction_step = cms.Path(process.reconstruction*process.tracksBeamHaloMuon)
process.endjob_step = cms.Path(process.endOfProcess)
process.out_step = cms.EndPath(process.output)

# Schedule definition
process.schedule = cms.Schedule(process.raw2digi_step,process.reconstruction_step,process.endjob_step,process.out_step)
