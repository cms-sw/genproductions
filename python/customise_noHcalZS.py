import FWCore.ParameterSet.Config as cms

def noHcalZS(process):

    process.simHcalDigis.markAndPass = cms.bool(True)
    process.simHcalDigis.HBlevel = cms.int32(-999)
    process.simHcalDigis.HElevel = cms.int32(-999)
    process.simHcalDigis.HOlevel = cms.int32(-999)
    process.simHcalDigis.HFlevel = cms.int32(-999)
    process.simHcalDigis.useConfigZSvalues = cms.int32(1)

    return(process)
