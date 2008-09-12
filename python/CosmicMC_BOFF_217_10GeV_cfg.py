#------------------------------------------------------------------------
# Production of cosmics, underground, full CMS acceptance
# CMSSW 2_1_7, Official prod stuff is included
# Last update: KH 12.09.08
# - E_min = 10 GeV, angular acceptance <60, 30M events
# - Global tag = COSMMC_21X incl. ECAL gain and DT calib 
# - keep special tracker TOF shift
# - Removed HCAl ZS 
# - ECAL settings for cosmics digitization
# - Include L1 emulator for all systems
#-----------------------------------------------------------------------

import FWCore.ParameterSet.Config as cms

process = cms.Process("runCosMuoGen")

process.maxEvents = cms.untracked.PSet(input = cms.untracked.int32(50))

process.configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source:/cvs_server/repositories/CMSSW/CMSSW/Configuration/Spring08Production/data/CosmicMC_BOFF_217_10GeV_cfg.py'),
    annotation = cms.untracked.string('Simulation for CRAFT data, underground B-field ON and 1 BX, special detector settings')
    )



#--------- Standard sequences

process.load('FWCore/MessageService/MessageLogger_cfi')

process.load("Configuration.StandardSequences.GeometryPilot2_cff")
process.load("Configuration.StandardSequences.MixingNoPileUp_cff")
process.load("Configuration.StandardSequences.VtxSmearedNoSmear_cff")

process.load("Configuration.StandardSequences.Generator_cff")
process.load("Configuration.StandardSequences.Simulation_cff")
process.load('Configuration.StandardSequences.Digi_cff')
process.load("Configuration.StandardSequences.DigiToRaw_cff")

process.load('Configuration.EventContent.EventContentCosmics_cff')

#--- random number generator (new in 21x) 

process.load('Configuration/StandardSequences/Services_cff')
process.RandomNumberGeneratorService.theSource.initialSeed = cms.untracked.uint32(12345688)

#--- non-collision events

process.g4SimHits.NonBeamEvent = cms.bool(True)
process.g4SimHits.Generator.ApplyEtaCuts = cms.bool(False)
process.psim = cms.Sequence(cms.SequencePlaceholder("randomEngineStateProducer")*process.g4SimHits)


#--- Cosmic Generator

process.load("GeneratorInterface.CosmicMuonGenerator.CMSCGENsource_cfi")
process.CosMuoGenSource.MinP = 10.
process.CosMuoGenSource.MaxTheta = 60.


#--- Conditions, needs special global tag for ECAL gain

process.load("Configuration.StandardSequences.FrontierConditions_GlobalTag_cff")
process.GlobalTag.globaltag =  'COSMMC_21X::All'


#--------- Force magnetic field to be 0 tesla for this sample
#          no need for MagneticField_cff

process.load("Configuration.StandardSequences.MagneticField_0T_cff")


#--------- Include L1 emulator

process.load('Configuration/StandardSequences/SimL1Emulator_cff')
process.load('L1Trigger.DTTrackFinder.dttfDigis_cfi')
process.dttfDigis.Open_LUTs = True

#-------- SPECIAL stuff for subsystems

# Pixel digitization: set the TOF window for digitizer to accept hits,
# window should stay fixed to 25 ns, shift of TOF for the strip tracker

process.load("SimTracker.SiPixelDigitizer.PixelDigi_cfi")
process.simSiPixelDigis.TofLowerCut = 18.5
process.simSiPixelDigis.TofUpperCut = 43.5

process.simSiStripDigis.CosmicDelayShift = 31.
process.simSiStripDigis.CouplingCostantPeak = cms.vdouble(0.94,0.03)

# Ecal digitization: change thresholds for selective readout and zero suppression
# ECAL gain needs special global tag

process.simEcalUnsuppressedDigis.cosmicsPhase = True
process.simEcalUnsuppressedDigis.cosmicsShift = 1.
process.simEcalDigis.ebDccAdcToGeV = cms.double(0.00875)
process.simEcalDigis.srpBarrelLowInterestChannelZS = cms.double(0.0153125)
process.EcalTrigPrimESProducer.DatabaseFile = cms.untracked.string('TPG_cosmics_ZS.txt.gz')

# Remove HCAL zero suppression

process.simHcalDigis.hbhe.level = -10000
process.simHcalDigis.ho.level   = -10000
process.simHcalDigis.hf.level   = -10000

#--------- Processes and output

process.output = cms.OutputModule("PoolOutputModule",
#    outputCommands = process.FEVTEventContent.outputCommands,
    fileName = cms.untracked.string('CosmicMC_BOFF_RAW_10GeV.root'),
    dataset = cms.untracked.PSet(
        dataTier = cms.untracked.string('GEN-SIM-RAW'),
        filterName = cms.untracked.string('')
    )
                                  )


#---------------------
# paths and sequences 
#---------------------
process.p0 = cms.Path(process.pgen)
process.p1 = cms.Path(process.psim)
process.p2 = cms.Path(process.pdigi)
process.p3 = cms.Path(process.SimL1Emulator)
process.p4 = cms.Path(process.DigiToRaw)
process.outpath = cms.EndPath(process.output)


process.schedule = cms.Schedule(process.p0,process.p1,process.p2,process.p3,process.p4,process.outpath)

