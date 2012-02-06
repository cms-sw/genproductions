import FWCore.ParameterSet.Config as cms

#from SimG4Core.Application.g4SimHits_cfi import *

def customise(process):

       process.load("SimG4Core.Application.g4SimHits_cfi")
       process.g4SimHits.Physics = cms.PSet(
        # NOTE : if you want EM Physics only,
        #        please select "SimG4Core/Physics/DummyPhysics" for type
        #        and turn ON DummyEMPhysics
        #
        type = cms.string('SimG4Core/Physics/QGSP_FTFP_BERT_EML95'),
        DummyEMPhysics = cms.bool(False),
        CutsPerRegion = cms.bool(True),
        DefaultCutValue = cms.double(1.0), ## cuts in cm
        G4BremsstrahlungThreshold = cms.double(0.5), ## cut in GeV
        Verbosity = cms.untracked.int32(0),
        # 1 will print cuts as they get set from DD
        # 2 will do as 1 + will dump Geant4 table of cuts
        MonopoleCharge       = cms.untracked.int32(1),
        MonopoleDeltaRay     = cms.untracked.bool(True),
        MonopoleMultiScatter = cms.untracked.bool(False),
        MonopoleTransport    = cms.untracked.bool(True),
        Region      = cms.string(' '),
	TrackingCut = cms.bool(True),
        SRType      = cms.bool(True),
        EMPhysics   = cms.untracked.bool(True),
        HadPhysics  = cms.untracked.bool(True),
        FlagBERT    = cms.untracked.bool(False),
        FlagCHIPS   = cms.untracked.bool(False),
        FlagFTF     = cms.untracked.bool(False),
        FlagGlauber = cms.untracked.bool(False),
        FlagHP      = cms.untracked.bool(False),
        GFlash = cms.PSet(
            GflashHistogram = cms.bool(False),
            GflashEMShowerModel = cms.bool(False),
            GflashHadronPhysics = cms.string('QGSP_BERT_EMV'),
            GflashHadronShowerModel = cms.bool(False)
        )
       )

       return(process)
