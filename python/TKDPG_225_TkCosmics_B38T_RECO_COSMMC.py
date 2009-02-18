import FWCore.ParameterSet.Config as cms

process = cms.Process("Rec")

#--------- Standard sequences
process.load("FWCore.MessageLogger.MessageLogger_cfi")
process.load("Configuration.StandardSequences.GeometryPilot2_cff")
process.load("Configuration.StandardSequences.ReconstructionCosmics_cff")
process.load("Configuration.StandardSequences.RawToDigi_cff")

#--- Conditions

process.load("Configuration.StandardSequences.FrontierConditions_GlobalTag_cff")
process.GlobalTag.globaltag = 'COSMMC_21X_V1::All'

#--------- Magnetic field Field should be ON for this sample
process.load("Configuration.StandardSequences.MagneticField_cff")

# event content: TrackingParticle added
process.load("Configuration.EventContent.EventContentCosmics_cff")
process.myeventcontent = cms.PSet(
     outputCommands = cms.untracked.vstring(
     'keep *_trackingtruthprod_*_*', 
         'keep *_electrontruth_*_*', 
         'keep *_mergedtruth_MergedTrackTruth_*'
	 )
 ) 
process.RECOSIMEventContent.outputCommands.extend(process.myeventcontent.outputCommands)

process.maxEvents = cms.untracked.PSet(
    input = cms.untracked.int32(-1)
)
process.source = cms.Source("PoolSource",
    firstFreeID = cms.untracked.uint32(131072),
 fileNames = cms.untracked.vstring(
'/store/mc/Summer08/TkAlCosmics4T/GEN-SIM-RAW/COSMMC_21X_V1_v1/0016/0A2B220B-C1C0-DD11-9290-0016368E0AE8.root'
)
)

#--- output module

process.FEVT = cms.OutputModule("PoolOutputModule",
    process.RECOSIMEventContent,
    dataset = cms.untracked.PSet(
        dataTier = cms.untracked.string('GEN-SIM-RECO')
    ),
    fileName = cms.untracked.string('Reco_TKCosmicMC_BON_225.root')
)


#--- Metadata

process.configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.2 $'),
    name = cms.untracked.string('$Source: /local/projects/CMSSW/rep/CMSSW/Configuration/GenProduction/python/TKDPG_22X_TkCosmic_B38T_Reco_cfg.py,v $'),
    annotation = cms.untracked.string('MC Cosmic Reco at 3.8 tesla')
)


#---------------------------------------
# paths 
#---------------------------------------
process.raw2digi_step = cms.Path(process.RawToDigi)
process.reconstruction_step = cms.Path(process.reconstructionCosmics)
process.outpath = cms.EndPath(process.FEVT)
process.schedule = cms.Schedule(process.raw2digi_step,
                                process.reconstruction_step,
                                process.outpath)
