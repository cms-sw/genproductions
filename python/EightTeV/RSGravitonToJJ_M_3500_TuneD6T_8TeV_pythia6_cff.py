import FWCore.ParameterSet.Config as cms

from Configuration.Generator.PythiaUED6TSettings_cfi import *

source = cms.Source("EmptySource")
generator = cms.EDFilter("Pythia6GeneratorFilter", 
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(1.0),
    comEnergy = cms.double(8000.0),
    crossSection = cms.untracked.double(0.0001227),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 3500.         !mass of RS Graviton', 
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

