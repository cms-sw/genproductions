import FWCore.ParameterSet.Config as cms

process = cms.Process("Rec")
process.load("FWCore.MessageLogger.MessageLogger_cfi")

process.load("Configuration.StandardSequences.MagneticField_cff")

process.load("CondCore.DBCommon.CondDBSetup_cfi")

process.load("Configuration.EventContent.EventContentCosmics_cff")

process.load("Configuration.EventContent.AlCaRecoOutput_cff")

process.load("Configuration.StandardSequences.FrontierConditions_GlobalTag_cff")

process.load("Configuration.StandardSequences.ReconstructionCosmics_cff")

process.load("Configuration.StandardSequences.RawToDigi_cff")

process.load("Configuration.StandardSequences.AlCaReco_cff")

process.load("Configuration.StandardSequences.Services_cff")
process.load("Configuration.StandardSequences.Geometry_cff")




process.maxEvents = cms.untracked.PSet(
    input = cms.untracked.int32(-1)
)
process.prefer("VolumeBasedMagneticFieldESProducer")
process.source = cms.Source("PoolSource",
    fileNames = cms.untracked.vstring('file:CosmicGenSimRawHLT4T.root')
   
)

process.FEVT = cms.OutputModule("PoolOutputModule",
    process.RECOSIMEventContent,
    dataset = cms.untracked.PSet(
        dataTier = cms.untracked.string('GEN-SIM-RECO')
    ),
    fileName = cms.untracked.string('file:RECOALCARECO4T.root')
)



process.configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.2 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/Examples/data/RecoCosmicsMC_4T_RECOALCARECO.cfg,v $'),
    annotation = cms.untracked.string('MC Cosmic Reco at 3.8 tesla')
)
process.options = cms.untracked.PSet(
    wantSummary = cms.untracked.bool(True)
)

process.poolALCARECOTkAlCosmics = cms.OutputModule("PoolOutputModule",
    process.OutALCARECOTkAlCosmics,
    dataset = cms.untracked.PSet(
        filterName = cms.untracked.string('ALCARECOTkAlCosmics'),
        dataTier = cms.untracked.string('ALCARECO')
    ),
    fileName = cms.untracked.string('file:ALCARECOTkAlCosmics4T.root')
)


process.prefer("GlobalTag")
process.raw2digi_step = cms.Path(process.RawToDigi)
process.reconstruction_step = cms.Path(process.reconstructionCosmics)
process.outpath = cms.EndPath(process.FEVT)
process.outPathALCARECOTkAlCosmics = cms.EndPath(process.poolALCARECOTkAlCosmics)
process.schedule = cms.Schedule(process.raw2digi_step,
                                process.reconstruction_step,
                                process.pathALCARECOTkAlCosmicsCosmicTF,
                                process.pathALCARECOTkAlCosmicsCTF,
                                process.pathALCARECOTkAlCosmicsRS,
                                process.outpath,process.outPathALCARECOTkAlCosmics)

process.GlobalTag.globaltag = 'STARTUP_V4::All'

