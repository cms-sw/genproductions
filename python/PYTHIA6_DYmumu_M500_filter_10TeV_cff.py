import FWCore.ParameterSet.Config as cms
from Configuration.GenProduction.PythiaUESettings_cfi import *

source = cms.Source("PythiaSource",
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(0.828),
    comEnergy = cms.untracked.double(10000.0),
    crossSection = cms.untracked.double(5.49e-2),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring(
            "MSEL        = 0   ! user defined processes",
            "MSUB(1)     = 1   ! ff -> gamma*/Z0/Z'",
            "MSTP(43)    = 3   ! complete Z0/gamma* interference",
            "CKIN(1)     = 500 ! min sqrt(s hat) (GeV)",
            "CKIN(2)     = -1  ! (no) max sqrt(s hat) (GeV)",
            "MDME(174,1) = 0   !Z decay into d dbar",        
            "MDME(175,1) = 0   !Z decay into u ubar",
            "MDME(176,1) = 0   !Z decay into s sbar",
            "MDME(177,1) = 0   !Z decay into c cbar",
            "MDME(178,1) = 0   !Z decay into b bbar",
            "MDME(179,1) = 0   !Z decay into t tbar",
            "MDME(182,1) = 0   !Z decay into e- e+",
            "MDME(183,1) = 0   !Z decay into nu_e nu_ebar",
            "MDME(184,1) = 1   !Z decay into mu- mu+",
            "MDME(185,1) = 0   !Z decay into nu_mu nu_mubar",
            "MDME(186,1) = 0   !Z decay into tau- tau+",
            "MDME(187,1) = 0   !Z decay into nu_tau nu_taubar",
        ),
        parameterSets = cms.vstring(
            "pythiaUESettings", 
            "processParameters"
        )
    )
)

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string("$Revision: 1.1 $"),
    name = cms.untracked.string("$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_DYmumu_M500_filter_10TeV_cff.py,v $"),
    annotation = cms.untracked.string("Drell-Yan -> mumu w/ Mmumu > 500 GeV at sqrt{s} = 10 TeV")
)

mumugenfilter = cms.EDFilter("MCParticlePairFilter",
    Status = cms.untracked.vint32(1, 1),
    MinPt = cms.untracked.vdouble(0.0, 0.0),
    MinEta = cms.untracked.vdouble(-2.5, -2.5),
    MaxEta = cms.untracked.vdouble(2.5, 2.5),
    ParticleCharge = cms.untracked.int32(0),
    ParticleID1 = cms.untracked.vint32(13),
    ParticleID2 = cms.untracked.vint32(13)
)

ProductionFilterSequence = cms.Sequence(mumugenfilter)

