import FWCore.ParameterSet.Config as cms


from Configuration.GenProduction.PythiaUESettings_cfi import *
source = cms.Source("PythiaSource",
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(1.0),
    comEnergy = cms.untracked.double(10000.0),
    crossSection = cms.untracked.double(4.09),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('MSEL       = 0      ', 
            'MSUB(142)  = 1      !f~f  -> W prime', 
            'PMAS(34,1) = 1000.  !mass of Wprime', 
            'CKIN(1)    = 400    !(D=2. GeV)', 
            "MDME(311,1) = 0    !W\' decay into dbar u", 
            "MDME(312,1) = 0    !W\' decay into dbar c", 
            "MDME(313,1) = 0    !W\' decay into dbar t", 
            "MDME(315,1) = 0    !W\' decay into sbar u", 
            "MDME(316,1) = 0    !W\' decay into sbar c", 
            "MDME(317,1) = 0    !W\' decay into sbar t", 
            "MDME(319,1) = 0    !W\' decay into bbar u", 
            "MDME(320,1) = 0    !W\' decay into bbar c", 
            "MDME(321,1) = 0    !W\' decay into bbar t", 
            "MDME(327,1) = 0    !W\' decay into e+ nu_e", 
            "MDME(328,1) = 1    !W\' decay into mu+ nu_mu", 
            "MDME(329,1) = 0    !W\' decay into tau+ nu_tau"),
        parameterSets = cms.vstring('pythiaUESettings', 
            'processParameters')
    )
)

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.4 $'),
    annotation = cms.untracked.string('PYTHIA6-Wprime of 1 TeV mass decaying into muon and neutrino at 10TeV  ')
,    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/Configuration/GenProduction/python/PYTHIA6_Exotica_WprimeMu_1TeV_10TeV_cff.py,v $'),
)

