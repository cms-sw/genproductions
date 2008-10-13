#--------------------------------------------------------------------------------------------------------
# Reco of simulated cosmics in CMSSW 2_1_10
# Based on recoT0DQM_EvContent_cfg.py for real data
# Last update: KH 12.09.08
#
# This one is for B=on, e.g. nominal field of 3.8T
#--------------------------------------------------------------------------------------------------------

import FWCore.ParameterSet.Config as cms

process = cms.Process("RecoCosmicMC")
process.load("FWCore.MessageLogger.MessageLogger_cfi")
#service = MessageLogger ("cerr")

process.maxEvents = cms.untracked.PSet(  input = cms.untracked.int32(-1) )
process.source = cms.Source("PoolSource",
#    fileNames = cms.untracked.vstring('..'
# local file
    fileNames=cms.untracked.vstring('file:CosmicMC_BOFF_RAW_4to10GeV.root'
                            )
)

#--- output module

process.load("Configuration.EventContent.EventContentCosmics_cff")

process.FEVT = cms.OutputModule("PoolOutputModule",
    process.FEVTEventContent,
    dataset = cms.untracked.PSet(dataTier = cms.untracked.string('RECO')),
    fileName = cms.untracked.string('reco_CosmicMC_BON_2110.root')
)

process.FEVT.outputCommands.append('keep CaloTowersSorted_calotoweroptmaker_*_*')
process.FEVT.outputCommands.append('keep CSCDetIdCSCALCTDigiMuonDigiCollection_muonCSCDigis_MuonCSCALCTDigi_*')
process.FEVT.outputCommands.append('keep CSCDetIdCSCCLCTDigiMuonDigiCollection_muonCSCDigis_MuonCSCCLCTDigi_*')
process.FEVT.outputCommands.append('keep CSCDetIdCSCComparatorDigiMuonDigiCollection_muonCSCDigis_MuonCSCComparatorDigi_*')
process.FEVT.outputCommands.append('keep CSCDetIdCSCCorrelatedLCTDigiMuonDigiCollection_csctfDigis_*_*')
process.FEVT.outputCommands.append('keep CSCDetIdCSCCorrelatedLCTDigiMuonDigiCollection_muonCSCDigis_MuonCSCCorrelatedLCTDigi_*')
process.FEVT.outputCommands.append('keep CSCDetIdCSCRPCDigiMuonDigiCollection_muonCSCDigis_MuonCSCRPCDigi_*')
process.FEVT.outputCommands.append('keep CSCDetIdCSCStripDigiMuonDigiCollection_muonCSCDigis_MuonCSCStripDigi_*')
process.FEVT.outputCommands.append('keep CSCDetIdCSCWireDigiMuonDigiCollection_muonCSCDigis_MuonCSCWireDigi_*')
process.FEVT.outputCommands.append('keep cscL1TrackCSCDetIdCSCCorrelatedLCTDigiMuonDigiCollectionstdpairs_csctfDigis_*_*')
process.FEVT.outputCommands.append('keep DTChamberIdDTLocalTriggerMuonDigiCollection_muonDTDigis_*_*')
process.FEVT.outputCommands.append('keep DTLayerIdDTDigiMuonDigiCollection_muonDTDigis_*_*')
process.FEVT.outputCommands.append('keep intL1CSCSPStatusDigisstdpair_csctfDigis_*_*')
process.FEVT.outputCommands.append('keep L1MuDTChambPhContainer_dttfDigis_*_*')
process.FEVT.outputCommands.append('keep L1MuDTChambThContainer_dttfDigis_*_*')
process.FEVT.outputCommands.append('keep L1MuDTTrackContainer_dttfDigis_DATA_*')
process.FEVT.outputCommands.append('keep PixelDigiedmDetSetVector_siPixelDigis_*_*')
process.FEVT.outputCommands.append('keep recoCandidatesOwned_caloTowersOpt_*_*')
process.FEVT.outputCommands.append('keep RPCDetIdRPCDigiMuonDigiCollection_muonRPCDigis_*_*')

#--- Metadata

process.configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/CosmicMC_BON_2110_RECO.py,v $'),
    annotation = cms.untracked.string('Reco of simulated cosmics with B=on')
)


#--- Standard Sequences

process.options = cms.untracked.PSet( wantSummary = cms.untracked.bool(True) ) ## default is false

process.load("Configuration.StandardSequences.Services_cff")

#Geometry, use GeometryPilot2 for cosmics
process.load("Configuration.StandardSequences.GeometryPilot2_cff")
 
# Conditions (Global Tag is used here):
process.load("Configuration.StandardSequences.FrontierConditions_GlobalTag_cff")
process.GlobalTag.globaltag = "COSMMC_21X::All"

# Magnetic field should be on
process.load("Configuration.StandardSequences.MagneticField_38T_cff")

# Raw to digi for simulated not for data
process.load("Configuration.StandardSequences.RawToDigi_cff")


#--- Reconstruction sequence for Cosmics
process.load("Configuration.StandardSequences.ReconstructionCosmics_cff")

  
#-- Path and sequences

process.p1 = cms.Path(process.RawToDigi*process.reconstructionCosmics)
process.outpath = cms.EndPath(process.FEVT)

