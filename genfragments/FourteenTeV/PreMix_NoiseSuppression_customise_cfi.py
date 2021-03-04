import FWCore.ParameterSet.Config as cms

def customise(process):

    # Turn off zero suppression in Ecal:
    process.GlobalTag.toGet = cms.VPSet(
            cms.PSet(record = cms.string("EcalSRSettingsRcd"),
                                                   tag = cms.string('EcalSRSettings_fullreadout_v01_mc'),
                                                   connect = cms.untracked.string("frontier://FrontierProd/CMS_COND_34X_ECAL")
                                      ))

    # Turn off noise generation:
    process.mix.digitizers.hcal.doNoise = cms.bool(False)
    process.mix.digitizers.hcal.doEmpty = False
    process.mix.digitizers.hcal.doHPDNoise = False
    process.mix.digitizers.hcal.doIonFeedback = False
    process.mix.digitizers.hcal.doThermalNoise = False

    process.mix.digitizers.ecal.doNoise = cms.bool(False)
    process.mix.digitizers.pixel.AddNoise = cms.bool(False)
    process.mix.digitizers.strip.AddNoise = cms.bool(False)
    process.simMuonCSCDigis.strips.doNoise = False
    process.simMuonCSCDigis.wires.doNoise = False
    process.simMuonDTDigis.onlyMuHits = True
    process.simMuonRPCDigis.Noise = False

    # Turn off zero suppression in Hcal:
    process.simHcalDigis.HBlevel = -10000
    process.simHcalDigis.HElevel = -10000
    process.simHcalDigis.HOlevel = -10000
    process.simHcalDigis.HFlevel = -10000
    process.simHcalDigis.useConfigZSvalues = 1
    
    return(process)
