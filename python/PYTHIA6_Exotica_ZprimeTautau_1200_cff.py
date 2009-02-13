import FWCore.ParameterSet.Config as cms


from Configuration.GenProduction.PythiaUESettings_cfi import *
source = cms.Source("PythiaSource",
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(1.0),
    comEnergy = cms.untracked.double(10000.0),
    crossSection = cms.untracked.double(15.7),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(32,1)= 1200.           !mass of Zprime', 
            'MSEL=0                    !(D=1) to select between full user control (0, then use MSUB) and some preprogrammed alternative', 
            'MSTP(44) = 3              !only select the Z process', 
            'MSUB(141) = 1             !ff  gamma z0 Z0', 
            'MDME(289,1)= 0            !d dbar', 
            'MDME(290,1)= 0            !u ubar', 
            'MDME(291,1)= 0            !s sbar', 
            'MDME(292,1)= 0            !c cbar', 
            'MDME(293,1)= 0            !b bar', 
            'MDME(294,1)= 0            !t tbar', 
            'MDME(295,1)= 0            !4th gen Q Qbar', 
            'MDME(296,1)= 0            !4th gen Q Qbar', 
            'MDME(297,1)= 0            !e e', 
            'MDME(298,1)= 0            !neutrino e e', 
            'MDME(299,1)= 0            ! mu mu', 
            'MDME(300,1)= 0            !neutrino mu mu', 
            'MDME(301,1)= 1            !tau tau', 
            'MDME(302,1)= 0            !neutrino tau tau', 
            'MDME(303,1)= 0            !4th generation lepton', 
            'MDME(304,1)= 0            !4th generation neutrino', 
            'MDME(305,1)= 0            !W W', 
            'MDME(306,1)= 0            !H  charged higgs', 
            'MDME(307,1)= 0            !Z', 
            'MDME(308,1)= 0            !Z', 
            'MDME(309,1)= 0            !sm higgs', 
            'MDME(310,1)= 0            !weird neutral higgs HA'),
        parameterSets = cms.vstring('pythiaUESettings', 
            'processParameters')
    )
)

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.0. %'),
    annotation = cms.untracked.string('default documentation string for ZprimeTautau_1200_cff.py'),
    name = cms.untracked.string('$Source: /local/projects/CMSSW/rep/CMSSW/Configuration/GenProduction/python/PYTHIA6_Exotica_ZprimeTautau_1200_cff.py,v $')
)


