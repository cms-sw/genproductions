import FWCore.ParameterSet.Config as cms

def customise_gensim(process): 

    # extended geometric acceptance (full CASTOR acceptance)

    process.g4SimHits.Generator.MinEtaCut = cms.double(-6.7)
    process.g4SimHits.Generator.MaxEtaCut = cms.double(6.7)

    # use HF shower library instead of GFlash parameterization

    process.g4SimHits.HCalSD.UseShowerLibrary = cms.bool(True)
    process.g4SimHits.HCalSD.UseParametrize = cms.bool(False)
    process.g4SimHits.HCalSD.UsePMTHits = cms.bool(False)
    process.g4SimHits.HCalSD.UseFibreBundleHits = cms.bool(False)
    process.g4SimHits.HFShower.ApplyFiducialCut = cms.bool(True)
    process.g4SimHits.HFShowerLibrary.ApplyFiducialCut = cms.bool(False)
      
    return(process)

def customise_digi(process):

    process.mix.mixObjects.mixCH = cms.PSet(
        input = cms.VInputTag(cms.InputTag("g4SimHits","CaloHitsTk"), cms.InputTag("g4SimHits","EcalHitsEB"), cms.InputTag("g4SimHits","EcalHitsEE"), cms.InputTag("g4SimHits","EcalHitsES"), cms.InputTag("g4SimHits","EcalTBH4BeamHits"), cms.InputTag("g4SimHits","HcalHits"), cms.InputTag("g4SimHits","HcalTB06BeamHits"), cms.InputTag("g4SimHits","ZDCHITS")),
        type = cms.string('PCaloHit'),
        subdets = cms.vstring('CaloHitsTk',
            'EcalHitsEB',      
            'EcalHitsEE',      
            'EcalHitsES',      
            'EcalTBH4BeamHits',
            'HcalHits',        
            'HcalTB06BeamHits',
            'ZDCHITS')         
    )
    process.calDigi = cms.Sequence(process.ecalDigiSequence+process.hcalDigiSequence)
    process.DigiToRaw = cms.Sequence(process.csctfpacker+process.dttfpacker+process.gctDigiToRaw+process.l1GtPack+process.l1GtEvmPack+process.siPixelRawData+process.SiStripDigiToRaw+process.ecalPacker+process.esDigiToRaw+process.hcalRawData+process.cscpacker+process.dtpacker+process.rpcpacker+process.rawDataCollector)

    return(process) 

def customise_validation(process):

    process.mix.mixObjects.mixCH = cms.PSet(
        input = cms.VInputTag(cms.InputTag("g4SimHits","CaloHitsTk"), cms.InputTag("g4SimHits","EcalHitsEB"), cms.InputTag("g4SimHits","EcalHitsEE"), cms.InputTag("g4SimHits","EcalHitsES"), cms.InputTag("g4SimHits","EcalTBH4BeamHits"), cms.InputTag("g4SimHits","HcalHits"), cms.InputTag("g4SimHits","HcalTB06BeamHits"), cms.InputTag("g4SimHits","ZDCHITS")),
        type = cms.string('PCaloHit'),
        subdets = cms.vstring('CaloHitsTk',
            'EcalHitsEB',      
            'EcalHitsEE',      
            'EcalHitsES',      
            'EcalTBH4BeamHits',
            'HcalHits',        
            'HcalTB06BeamHits',
            'ZDCHITS')         
    )

    return(process)
