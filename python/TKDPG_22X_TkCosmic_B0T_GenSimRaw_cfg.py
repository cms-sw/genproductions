import FWCore.ParameterSet.Config as cms

process = cms.Process("GenSimRaw")

process.configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.2 $'),
    name = cms.untracked.string('$Source:/cvs_server/repositories/CMSSW/CMSSW/Configuration/Spring08Production/data/CosmicMC_BON_217_4to10GeV_cfg.py'),
    annotation = cms.untracked.string('Simulation of cosmic muons in Tracker with B field off')
    )



process.maxEvents = cms.untracked.PSet(
    input = cms.untracked.int32(500)
)

#--------- Standard sequences
process.load("FWCore.MessageService.MessageLogger_cfi")
process.load("Configuration.StandardSequences.GeometryPilot2_cff")
process.load("Configuration.StandardSequences.MixingNoPileUp_cff")
process.load("Configuration.StandardSequences.VtxSmearedNoSmear_cff")
process.load("Configuration.StandardSequences.Simulation_cff")
process.load("Configuration.StandardSequences.DigiToRaw_cff")
process.load("Configuration.EventContent.EventContentCosmics_cff")

#--- random number generator (new in 21x) 

process.load('Configuration/StandardSequences/Services_cff')
process.RandomNumberGeneratorService.theSource.initialSeed = cms.untracked.uint32(12345688)


#--- non-collision events

process.g4SimHits.NonBeamEvent = True
process.g4SimHits.Generator.ApplyEtaCuts = False
process.psim = cms.Sequence(cms.SequencePlaceholder("randomEngineStateProducer")*process.g4SimHits)



#--- Cosmic Generator
process.load("GeneratorInterface.CosmicMuonGenerator.CMSCGENsource_cfi")
process.load("GeneratorInterface.GenFilters.CosmicGenFilterHelix_cff")
process.CosMuoGenSource.MinP = 4.


#--- Conditions

process.load("Configuration.StandardSequences.FrontierConditions_GlobalTag_cff")
process.GlobalTag.globaltag =  'COSMMC_21X_V1::All'

#--------- Magnetic field Field should be OFF for this sample
process.load("Configuration.StandardSequences.MagneticField_0T_cff")


#--------- Include L1 emulator
process.load("Configuration.StandardSequences.SimL1Emulator_cff")
process.load("L1Trigger.DTTrackFinder.dttfDigis_cfi")
process.dttfDigis.Open_LUTs = True

#-------- SPECIAL stuff for subsystems

#Pixel digitization
process.load("SimTracker.SiPixelDigitizer.PixelDigi_cfi")
process.simSiPixelDigis.TofLowerCut = 18.5
process.simSiPixelDigis.TofUpperCut = 43.5
process.simSiStripDigis.CosmicDelayShift = 31.


# Ecal digitization: change thresholds for selective readout and zero suppression

process.simEcalUnsuppressedDigis.cosmicsPhase = True
process.simEcalUnsuppressedDigis.cosmicsShift = 1.
process.simEcalDigis.ebDccAdcToGeV = cms.double(0.00875)
process.simEcalDigis.srpBarrelLowInterestChannelZS = cms.double(0.0153125)
process.EcalTrigPrimESProducer.DatabaseFile = cms.untracked.string('TPG_cosmics_ZS.txt.gz')


process.FEVT = cms.OutputModule("PoolOutputModule",
    fileName = cms.untracked.string('TkCosmicGenSimRaw0T.root'),
    dataset = cms.untracked.PSet(
    dataTier = cms.untracked.string('GEN-SIM-RAW'),
     filterName = cms.untracked.string('')
    ),    
    SelectEvents = cms.untracked.PSet(
        SelectEvents = cms.vstring('c')
	)
    )


#---------------------------------------
# paths 
#---------------------------------------
process.c = cms.Path(process.cosmicInTracker*process.psim*process.pdigi*process.SimL1Emulator*process.DigiToRaw)
process.outpath = cms.EndPath(process.FEVT)
