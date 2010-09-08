import FWCore.ParameterSet.Config as cms


from Configuration.Generator.PythiaUEZ2Settings_cfi import *

generator = cms.EDFilter("Pythia6GeneratorFilter",
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(1.0),
    comEnergy = cms.double(7000.0),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)=700.0        !mass of RS Graviton', 
            'MSEL=0                  !(D=1)', 
            'PARP(50) = 0.379  ! Dimensionless coupling constant for RS G*', 
            'MSTJ(41)=1              !Switch off Pythia QED bremsshtrahlung', 
            'MSUB(391)=1             !', 
            'MSUB(392)=1             !', 
            '5000039:ALLOFF           ', 
            '5000039:ONIFMATCH 23 23  ', 
            '23:ALLOFF                ', 
            '23:ONIFANY 13 13         '),
        parameterSets = cms.vstring('pythiaUESettings', 
            'processParameters')
    )
)

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    annotation = cms.untracked.string('RS1GravitonZZ4Mu_700GeV_007 at 7TeV'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_Exotica_RS1GravitonZZ4Mu_700GeV_007.py,v $')
)

ProductionFilterSequence = cms.Sequence(generator)

