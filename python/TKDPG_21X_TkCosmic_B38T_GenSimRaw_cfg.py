import FWCore.ParameterSet.Config as cms

process = cms.Process("GenSimRaw")
process.load("FWCore.MessageService.MessageLogger_cfi")
process.load("Configuration.StandardSequences.SimulationRandomNumberGeneratorSeeds_cff")
process.load("GeneratorInterface.CosmicMuonGenerator.CMSCGENsource_cfi")
process.load("GeneratorInterface.GenFilters.CosmicGenFilterHelix_cff")
process.load("Configuration.StandardSequences.Simulation_cff")
process.load("Configuration.StandardSequences.Reconstruction_cff")
process.load("SimTracker.SiPixelDigitizer.PixelDigi_cfi")
process.load("Configuration.StandardSequences.MagneticField_cff")
process.load("Configuration.StandardSequences.MixingNoPileUp_cff")
process.load("Configuration.StandardSequences.VtxSmearedNoSmear_cff")
process.load("Configuration.StandardSequences.FrontierConditions_GlobalTag_cff")
process.load("Configuration.StandardSequences.DigiToRaw_cff")
process.load("Configuration.StandardSequences.L1Emulator_cff")
process.load("Configuration.EventContent.EventContent_cff")
process.load("Configuration.StandardSequences.Geometry_cff")
process.load("L1Trigger.DTTrackFinder.dttfDigis_cfi")

process.maxEvents = cms.untracked.PSet(
    input = cms.untracked.int32(1000)
)
process.options = cms.untracked.PSet(
    wantSummary = cms.untracked.bool(True)
)

process.g4SimHits.NonBeamEvent = True
process.g4SimHits.Generator.ApplyEtaCuts = False

process.FEVT = cms.OutputModule("PoolOutputModule",
    fileName = cms.untracked.string('TkCosmicGenSimRaw.root'),
    SelectEvents = cms.untracked.PSet(
        SelectEvents = cms.vstring('c')
    ),
    outputCommands = cms.untracked.vstring('keep *', 
        'drop FEDRawDataCollection_*_*_*', 
        'keep FEDRawDataCollection_rawDataCollector__GenSimRaw')
)


process.GlobalTag.globaltag = 'STARTUP_V7::All'

process.c = cms.Path(process.cosmicInTracker*process.g4SimHits*process.pdigi*process.L1Emulator*process.DigiToRaw)
process.outpath = cms.EndPath(process.FEVT)

process.CosMuoGenSource.MinP = 4.
process.simSiPixelDigis.TofLowerCut = 18.5
process.simSiPixelDigis.TofUpperCut = 43.5
process.simSiStripDigis.CosmicDelayShift = 31.
process.dttfDigis.Open_LUTs = True

