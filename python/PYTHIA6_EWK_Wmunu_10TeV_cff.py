import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
source = cms.Source("PythiaSource",
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(0.691),
    comEnergy = cms.untracked.double(10000.0),
    crossSection = cms.untracked.double(11865.),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('MSEL        = 0    !User defined processes', 
                                        'MSUB(2)     = 1    !W production', 
                                        'MDME(190,1) = 0    !W decay into dbar u', 
                                        'MDME(191,1) = 0    !W decay into dbar c', 
                                        'MDME(192,1) = 0    !W decay into dbar t', 
                                        'MDME(194,1) = 0    !W decay into sbar u', 
                                        'MDME(195,1) = 0    !W decay into sbar c', 
                                        'MDME(196,1) = 0    !W decay into sbar t', 
                                        'MDME(198,1) = 0    !W decay into bbar u', 
                                        'MDME(199,1) = 0    !W decay into bbar c', 
                                        'MDME(200,1) = 0    !W decay into bbar t', 
                                        'MDME(205,1) = 0    !W decay into bbar tp', 
                                        'MDME(206,1) = 0    !W decay into e+ nu_e', 
                                        'MDME(207,1) = 1    !W decay into mu+ nu_mu', 
                                        'MDME(208,1) = 0    !W decay into tau+ nu_tau'),
        # This is a vector of ParameterSet names to be read, in this order
        parameterSets = cms.vstring('pythiaUESettings', 
            'processParameters')
    )
)

mugenfilter = cms.EDFilter("PythiaFilter",
    Status = cms.untracked.int32(3),
    MotherID = cms.untracked.int32(24),
    MinPt = cms.untracked.double(0.0),
    ParticleID = cms.untracked.int32(13),
    MaxEta = cms.untracked.double(2.5),
    MinEta = cms.untracked.double(-2.5)
)

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EWK_Wmunu_10TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6-EWK W to muon and neutrino at 10TeV')
)

ProductionFilterSequence = cms.Sequence(mugenfilter)
