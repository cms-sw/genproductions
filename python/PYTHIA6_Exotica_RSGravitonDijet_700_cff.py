import FWCore.ParameterSet.Config as cms


from Configuration.GenProduction.PythiaUESettings_cfi import *
source = cms.Source("PythiaSource",
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(1.0),
    comEnergy = cms.untracked.double(10000.0),
    crossSection = cms.untracked.double(45.2),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 700.         !mass of RS Graviton', 
            'PARP(50) = 0.54           ! 0.54 == c=0.1 (k/M_PL=0.1)', 
            'MSEL=0                    !(D=1) to select between full user control (0, then use MSUB) and some preprogrammed alternative', 
            'MSUB(391)=1               ! q qbar -> G* ', 
            'MSUB(392)=1               ! g g -> G*', 
            '5000039:ALLOFF            ! Turn off all decays of G*',         
            '5000039:ONIFANY 1 2 3 4 5 21  !Turn on the deays u ubar, d dbar, s sbar, c cbar, b bar, g g'),
        parameterSets = cms.vstring('pythiaUESettings', 
            'processParameters')
    )
)

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1. %'),
    annotation = cms.untracked.string('default documentation string for RSGravitonDijet_700_cff.py'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/Configuration/GenProduction/python/PYTHIA6_Exotica_RSGravitonDijet_700_cff.py,v $')
)

