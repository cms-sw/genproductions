import FWCore.ParameterSet.Config as cms

# Turn of MC dependence in pat sequence
def removePatMCMatch(process):
    process.prod.remove(process.genPartons)
    process.prod.remove(process.heavyIonCleanedGenJets)
    process.prod.remove(process.hiPartons)
    process.prod.remove(process.patJetGenJetMatch)
    process.prod.remove(process.patJetPartonMatch)
    
    process.patJets.addGenPartonMatch   = False
    process.patJets.embedGenPartonMatch = False
    process.patJets.genPartonMatch      = ''
    process.patJets.addGenJetMatch      = False
    process.patJets.genJetMatch      = ''
    process.patJets.getJetMCFlavour     = False
    process.patJets.JetPartonMapSource  = ''
    return process

# Top Config to turn off all mc dependence
def disableMC(process):
    process.prod.remove(process.heavyIon)
    removePatMCMatch(process)
    return process

def hltFromREDIGI(process):
    process.hltanalysis.HLTProcessName      = "REDIGI"
    process.hltanalysis.l1GtObjectMapRecord = cms.InputTag("hltL1GtObjectMap::REDIGI")
    process.hltanalysis.l1GtReadoutRecord   = cms.InputTag("hltGtDigis::REDIGI")
    process.hltanalysis.hltresults          = cms.InputTag("TriggerResults::REDIGI")   
    return process

def overrideGlobalTag(process):
    process.GlobalTag.toGet = cms.VPSet(
        cms.PSet(record = cms.string("EcalSRSettingsRcd"),
                 tag = cms.string("EcalSRSettings_fullreadout_v01_mc"),
                 connect = cms.untracked.string("frontier://FrontierPrep/CMS_COND_ECAL")
                 ),        
        cms.PSet(record = cms.string("BeamSpotObjectsRcd"),
                 tag = cms.string("Realistic2.76ATeVCollisions_STARTUP_v0_mc"),
                 connect = cms.untracked.string("frontier://FrontierProd/CMS_COND_31X_BEAMSPOT")
                 ),        
        cms.PSet(record = cms.string("HeavyIonRcd"),
                 tag = cms.string("CentralityTable_HFhits40_AMPTOrgan_v0_offline"),
                 connect = cms.untracked.string("frontier://FrontierProd/CMS_COND_31X_PHYSICSTOOLS"),
                 label = cms.untracked.string("HFhitsAMPT_Organ")
                 ),
        cms.PSet(record = cms.string("HeavyIonRcd"),
                 tag = cms.string("CentralityTable_PixelHits40_AMPTOrgan_v0_offline"),
                 connect = cms.untracked.string("frontier://FrontierProd/CMS_COND_31X_PHYSICSTOOLS"),
                 label = cms.untracked.string("PixelHitsAMPT_Organ")
                 ),
        
        cms.PSet(record = cms.string("HeavyIonRcd"),
                 tag = cms.string("CentralityTable_HFhits40_HydjetBass_v0_offline"),
                 connect = cms.untracked.string("frontier://FrontierProd/CMS_COND_31X_PHYSICSTOOLS"),
                 label = cms.untracked.string("HFhitsHydjet_Bass")
                                    ),
        cms.PSet(record = cms.string("HeavyIonRcd"),
                 tag = cms.string("CentralityTable_PixelHits40_HydjetBass_v0_offline"),
                 connect = cms.untracked.string("frontier://FrontierProd/CMS_COND_31X_PHYSICSTOOLS"),
                 label = cms.untracked.string("PixelHitsHydjet_Bass")
                 ),
        
        cms.PSet(record = cms.string("HeavyIonRcd"),
                 #                 tag = cms.string("CentralityTable_HFhits40_AMPTPiano_v0_offline"),
                 tag = cms.string("CentralityTable_HFhits40_AMPTPiano_v0_mc"),
                 connect = cms.untracked.string("frontier://FrontierProd/CMS_COND_31X_PHYSICSTOOLS"),
                 label = cms.untracked.string("HFhitsAMPT_Piano")
                 ),
        cms.PSet(record = cms.string("HeavyIonRcd"),
                 #                 tag = cms.string("CentralityTable_PixelHits40_AMPTPiano_v0_offline"),
                 tag = cms.string("CentralityTable_PixelHits40_AMPTPiano_v0_mc"),
                 connect = cms.untracked.string("frontier://FrontierProd/CMS_COND_31X_PHYSICSTOOLS"),
                 label = cms.untracked.string("PixelHitsAMPT_Piano")
                 ),
        
        cms.PSet(record = cms.string("HeavyIonRcd"),
                 #                 tag = cms.string("CentralityTable_HFhits40_HydjetGuitar_v0_offline"),
                 tag = cms.string("CentralityTable_HFhits40_HydjetGuitar_v0_mc"),
                 connect = cms.untracked.string("frontier://FrontierProd/CMS_COND_31X_PHYSICSTOOLS"),
                 label = cms.untracked.string("HFhitsHydjet_Guitar")
                 ),

        cms.PSet(record = cms.string("HeavyIonRcd"),
                 #                 tag = cms.string("CentralityTable_HFhits40_HydjetGuitar_v0_offline"),
                 tag = cms.string("CentralityTable_PixelHits40_HydjetGuitar_v0_mc"),
                 connect = cms.untracked.string("frontier://FrontierProd/CMS_COND_31X_PHYSICSTOOLS"),
                 label = cms.untracked.string("PixelHitsHydjet_Guitar")
                 ),
        

#==================== DATA ONLY, FIXED RUN TAGS =====================================

        cms.PSet(record = cms.string("HeavyIonRcd"),
                 tag = cms.string("CentralityTable_PixelHits40_AMPTOrgan_v0_r150590_mc"),
                 connect = cms.untracked.string("frontier://FrontierPrep/CMS_COND_PHYSICSTOOLS"),
                 label = cms.untracked.string("PixelHits")
                 ),
        
        cms.PSet(record = cms.string("HeavyIonRcd"),
                 tag = cms.string("CentralityTable_HFhits40_AMPTOrgan_v0_r150590_mc"),
                 connect = cms.untracked.string("frontier://FrontierPrep/CMS_COND_PHYSICSTOOLS"),                 
                 label = cms.untracked.string("HFhits")
                 ),
        
        )
    
    return process


def overrideCentrality(process):    
    overrideGlobalTag(process)
    return process

                              
