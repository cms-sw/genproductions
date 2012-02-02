import FWCore.ParameterSet.Config as cms


from Configuration.Generator.PythiaUED6TSettings_cfi import *

source = cms.Source("EmptySource")
generator = cms.EDFilter("Pythia6GeneratorFilter", 
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(1.0),
    comEnergy = cms.double(8000.0),
    crossSection = cms.untracked.double(132.0),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 500.         !mass of RS Graviton', 
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

ProductionFilterSequence = cms.Sequence(generator) 

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1. %'),
    annotation = cms.untracked.string('default documentation string for RSGravitonDijet_500_8TeV_TuneD6Tcff.py'),
    name = cms.untracked.string('$Source: /cvs/CMSSW/CMSSW/Configuration/GenProduction/python/Attic/RSGravitonToJJ_M_500_TuneD6T_8TeV_pythia6_cff.py,v $')
)

