#--------------------------------------------------------------------------------------------------------
# Reco of simulated cosmics in CMSSW 228
# Last update: KH & AR 29.04.09 
#  * event content changed to RECOSIM
#  * Bfield for RECO is ON, same as for generation
#--------------------------------------------------------------------------------------------------------

import FWCore.ParameterSet.Config as cms

process = cms.Process("RecoCosmicMC")
# process.load("FWCore.MessageLogger.MessageLogger_cfi")


process.load("CondCore.DBCommon.CondDBSetup_cfi")


process.maxEvents = cms.untracked.PSet(  input = cms.untracked.int32(-1) )
process.source = cms.Source("PoolSource",
# local file
   fileNames=cms.untracked.vstring('file:CosmicMC_BON_229_RAW_CSC.root'
                            )
)

#--- output module

process.load("Configuration.EventContent.EventContentCosmics_cff")

process.FEVT = cms.OutputModule("PoolOutputModule",
    process.RECOSIMEventContent,
    dataset = cms.untracked.PSet(dataTier = cms.untracked.string('RECO')),
    fileName = cms.untracked.string('reco_CosmicMC_BON_229_CSC_RECOSIM.root')
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

##### Content of RECOSIM ??

##StripDigiSimLinkedmDetSetVector_simMuonCSCDigis_MuonCSCStripDigiSimLinks_runCosMuoGen.
##SimTracks_g4SimHits__runCosMuoGen.
##recoGenJets_iterativeCone5GenJets__runCosMuoGen.
##recoGenJets_sisCone7GenJets__runCosMuoGen.
##recoGenJets_sisCone5GenJets__runCosMuoGen.
##recoGenJets_kt6GenJets__runCosMuoGen.
##recoGenJets_kt4GenJets__runCosMuoGen.
##recoGenMETs_genMetNoNuBSM__runCosMuoGen.
##recoGenMETs_genMet__runCosMuoGen.
##recoGenParticles_genParticles__runCosMuoGen.
##recoMETs_genMetIC5GenJets__runCosMuoGen.
##edmHepMCProduct_source__runCosMuoGen.
##SimVertexs_g4SimHits__runCosMuoGen.
##DTLayerIdDTDigiSimLinkMuonDigiCollection_simMuonDTDigis__runCosMuoGen.
##StripDigiSimLinkedmDetSetVector_simMuonCSCDigis_MuonCSCWireDigiSimLinks_runCosMuoGen.
##RPCDigiSimLinkedmDetSetVector_simMuonRPCDigis_RPCDigiSimLink_runCosMuoGen.
##recoPdfInfo_genEventPdfInfo__runCosMuoGen.
##edmTriggerResults_TriggerResults__runCosMuoGen.
##ints_genParticles__runCosMuoGen.
##double_genEventWeight__runCosMuoGen.
##double_genEventScale__runCosMuoGen.
##int_genEventProcID__runCosMuoGen.


#--- Metadata

process.configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.20 $'),
    name = cms.untracked.string('$Source: CosmicMC_BON_227_RECO.py,v $'),
    annotation = cms.untracked.string('Reco of simulated cosmics with B=on')
)


#--- Standard Sequences

process.options = cms.untracked.PSet( wantSummary = cms.untracked.bool(True) ) ## default is false

process.load("Configuration.StandardSequences.Services_cff")

#Geometry, use GeometryPilot2 for cosmics
process.load("Configuration.StandardSequences.GeometryPilot2_cff")
 
# Conditions (Global Tag is used here):

process.load("Configuration.StandardSequences.FrontierConditions_GlobalTag_cff")
process.GlobalTag.globaltag = "COSMMC_22X_V6::All"

# Magnetic field should be on

process.load("Configuration.StandardSequences.MagneticField_38T_UpdatedMap_cff")
# trick to make it work with newnew magfield (not in 229)
process.VolumeBasedMagneticFieldESProducer.version='grid_1103l_090322_3_8t'


# Raw to digi for simulated not for data

process.load("Configuration.StandardSequences.RawToDigi_cff")


#--- Reconstruction sequence for Cosmics

process.load("Configuration.StandardSequences.ReconstructionCosmics_cff")

#-- Path and sequences

process.p1 = cms.Path(process.RawToDigi*process.reconstructionCosmics)
process.outpath = cms.EndPath(process.FEVT)

