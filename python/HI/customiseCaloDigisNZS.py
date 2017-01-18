import FWCore.ParameterSet.Config as cms

##############################################################################
def customiseHcalNZS(process):
    process.hcalRawData.HBHE = cms.untracked.InputTag("simHcalUnsuppressedDigis")
    process.hcalRawData.HF = cms.untracked.InputTag("simHcalUnsuppressedDigis")
    process.hcalRawData.HO = cms.untracked.InputTag("simHcalUnsuppressedDigis")
    process.hcalRawData.ZDC = cms.untracked.InputTag("simHcalUnsuppressedDigis")

    return process


##############################################################################
def customiseEcalNZS(process):
    process.simEcalDigis.srpBarrelLowInterestChannelZS = cms.double(-1.e9)
    process.simEcalDigis.srpEndcapLowInterestChannelZS = cms.double(-1.e9)

    return process


##############################################################################
def customiseCaloNZS(process):
    process = customiseHcalNZS(process)
    process = customiseEcalNZS(process)

    return process
