import FWCore.ParameterSet.Config as cms
process = cms.Process("Rec")

process.load("Configuration.StandardSequences.FrontierConditions_GlobalTag_cff")

process.load("Configuration.StandardSequences.MagneticField_cff")

process.load("Configuration.StandardSequences.GeometryPilot2_cff")
process.load('Configuration.StandardSequences.RawToDigi_cff')
process.load('Configuration.StandardSequences.DigiToRaw_cff')

process.load('Configuration.EventContent.EventContent_cff')
process.load('Configuration.StandardSequences.Reconstruction_cff')
process.source = cms.Source("PoolSource",
    useCSA08Kludge = cms.untracked.bool(True),
    fileNames = cms.untracked.vstring(
'MuPtX_GEN_SIM_DIGI_L1_DIGI2RAW_HLT.root')
)



process.GlobalTag.connect = "frontier://FrontierProd/CMS_COND_21X_GLOBALTAG"
process.prefer("GlobalTag")
process.GlobalTag.globaltag = 'STARTUP_V7::All'




process.out_step = cms.OutputModule("PoolOutputModule",
    fileName = cms.untracked.string(
'MuPt11_cfi_RECO.root'
    ),
dataset = cms.untracked.PSet(
        filterName = cms.untracked.string('STARTUP_V7'),
        dataTier = cms.untracked.string('RECO')
    )
)





process.raw2digi_step = cms.Path(process.RawToDigi)
process.reconstruction_step = cms.Path(process.reconstruction)
process.outpath = cms.EndPath(process.out_step)
process.schedule = cms.Schedule(process.raw2digi_step,process.reconstruction_step,process.outpath)




