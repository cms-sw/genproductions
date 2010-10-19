##############################################################################
def customise(process):
    process.hcalRawData.HBHE = cms.untracked.InputTag("simHcalUnsuppressedDigis")
    process.hcalRawData.HF = cms.untracked.InputTag("simHcalUnsuppressedDigis")
    process.hcalRawData.HO = cms.untracked.InputTag("simHcalUnsuppressedDigis")
    process.hcalRawData.ZDC = cms.untracked.InputTag("simHcalUnsuppressedDigis")

    return process
