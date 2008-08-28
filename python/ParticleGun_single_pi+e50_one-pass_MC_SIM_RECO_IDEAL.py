import FWCore.ParameterSet.Config as cms

process = cms.Process("FULLPROD")

process.load("Configuration.StandardSequences.SimulationRandomNumberGeneratorSeeds_cff")
process.load("Configuration.StandardSequences.VtxSmearedGauss_cff")
process.load("Configuration.StandardSequences.Simulation_cff")
process.load("Configuration.StandardSequences.GeometryPilot2_cff")
process.load("Configuration.StandardSequences.Digi_cff")
process.load("Configuration.StandardSequences.MagneticField_cff")
process.load("Configuration.StandardSequences.MixingNoPileUp_cff")

process.load("Configuration.StandardSequences.Reconstruction_cff")

process.load("Configuration.StandardSequences.FrontierConditions_GlobalTag_cff")
process.GlobalTag.globaltag = 'IDEAL_V9::All'


process.maxEvents = cms.untracked.PSet(
    input = cms.untracked.int32(1000)
)

process.source = cms.Source("FlatRandomEGunSource",
    PGunParameters = cms.untracked.PSet(
        # you can request more than 1 particle
        PartID = cms.untracked.vint32(211),
        MaxEta = cms.untracked.double(5.1),
        MaxPhi = cms.untracked.double(3.14159265359),
        MinEta = cms.untracked.double(-5.1),
        MinE   = cms.untracked.double(50.0),
        MinPhi = cms.untracked.double(-3.14159265359),
        MaxE   = cms.untracked.double(50.0)
    ),
    firstRun = cms.untracked.uint32(1),
###    AddAntiParticle = cms.untracked.bool(True) 
)

process.VtxSmeared = cms.EDFilter("GaussEvtVtxGenerator",
    MeanX = cms.double(0.0),
    MeanY = cms.double(0.0),
    MeanZ = cms.double(0.0),
    SigmaY = cms.double(0.0001),
    SigmaX = cms.double(0.0001),
    SigmaZ = cms.double(0.0001),
    TimeOffset = cms.double(0.0),
    src = cms.InputTag("source")
)


process.USER = cms.OutputModule("PoolOutputModule",
    outputCommands = cms.untracked.vstring('keep *',
        'drop EBDataFramesSorted_*_*_*',
        'drop EEDataFramesSorted_*_*_*',
        'drop ESDataFramesSorted_*_*_*',
        'drop EBSrFlagsSorted_*_*_*',
        'drop EESrFlagsSorted_*_*_*',
        'drop EcalTriggerPrimitiveDigisSorted_*_*_*',
        'drop CrossingFrame_*_*_*',
        'drop HcalUnpackerReport_*__*',
        'drop HOTriggerPrimitiveDigisSorted_*__*',
        'drop HcalCalibDataFramesSorted_*__*',
        'drop HBHEDataFramesSorted_*_*_*',
        'drop HFDataFramesSorted_*_*_*',
        'drop HODataFramesSorted_*_*_*',
        'drop ZDCDataFramesSorted_*_*_*',
        'drop HcalTriggerPrimitiveDigisSorted_*_*_*',
        'drop FEDRawDataCollection_*__*',
        'drop PSimHits_*_*_*',
        'drop PCaloHits_g4SimHits_Castor_*',
        'drop PCaloHits_g4SimHits_CaloHitsTk_*',
        'drop PCaloHits_g4SimHits_EcalTBH4BeamHits_*',
        'drop PCaloHits_g4SimHits_HcalTB06BeamHits_*',
        'drop PCaloHits_g4SimHits_EcalHitsEB_*',
        'drop PCaloHits_g4SimHits_EcalHitsEE_*',
        'drop PCaloHits_g4SimHits_EcalHitsES_*',
        'drop PCaloHits_g4SimHits_CastorFI_*',
        'drop PCaloHits_g4SimHits_CastorTU_*',
        'drop PCaloHits_g4SimHits_CastorBU_*',
        'drop PCaloHits_g4SimHits_CastorPL_*',
        'drop PCaloHits_g4SimHits_ZDCHITS_*',
        'drop PSimHitCrossingFrame_*_*_*',
        'drop PCaloHitCrossingFrame_*_*_*',
        'drop SimTracks_*_*_*',
        'drop SimTrackCrossingFrame_*_*_*',
        'drop SimVertexs_*_*_*',
        'drop SimVertexCrossingFrame_*_*_*',
        'drop bool_VtxSmeared_*_*',
        'drop edmHepMCProductCrossingFrame_*_*_*',
        'drop edmTriggerResults_*_*_*',
        'drop EBDigiCollection_*_*_*',
        'drop EEDigiCollection_*_*_*',
        'drop EBDigiCollection_*__*',
        'drop EEDigiCollection_*__*',
        'drop EcalUncalibratedRecHitsSorted_*_*_*',
        'drop CrossingFramePlaybackInfo_*__*'),
    fileName = cms.untracked.string('output.root')
)

#--- 1st step: Sim+Digi+Raw

process.g4SimHits.UseMagneticField = False

process.simHcalDigis.hbhe.level = -10000
process.simHcalDigis.ho.level   = -10000
process.simHcalDigis.hf.level   = -10000
process.simHcalUnsuppressedDigis.doNoise = False

process.load("EventFilter.HcalRawToDigi.HcalDigiToRaw_cfi")

process.p1 = cms.Path(process.VtxSmeared*process.g4SimHits*process.mix*process.simHcalUnsuppressedDigis*process.simHcalDigis*process.hcalRawData)

### 2d step: Digi+Reco

process.load("EventFilter.HcalRawToDigi.HcalRawToDigi_cfi")
process.hcalDigis.InputLabel = cms.InputTag('hcalRawData')

process.p2 = cms.Path(process.hcalDigis*process.hbhereco*process.horeco*process.hfreco)


### Full schedule

process.outpath = cms.EndPath(process.USER)
process.schedule = cms.Schedule(process.p1,process.p2,process.outpath)

