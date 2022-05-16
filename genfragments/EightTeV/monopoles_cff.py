import FWCore.ParameterSet.Config as cms

from Configuration.Generator.PythiaUEZ2starSettings_cfi import *

generator = cms.EDFilter("Pythia6GeneratorFilter",
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(1.0),
    crossSection = cms.untracked.double(72700000000),
    comEnergy = cms.double(8000.0),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring(
          'MSEL=0',
          'MSUB(301)=1',
          'KFPR(301,1)=4110000',
          'KFPR(301,2)=4110000',
          'CHAF(500,1)=monopole',
          'CHAF(500,2)=anti-monopole',
          'PMAS(500,1)=300.',
          'MDCY(500,1)=0',
          'KCHG(500,4)=4110000',
          'KCHG(500,1)=3',
          'KCHG(500,2)=0',
          'KCHG(500,3)=1',
          'CKIN(1)=350.0'
        ),
        # This is a vector of ParameterSet names to be read, in this order
        parameterSets = cms.vstring('pythiaUESettings', 
            'processParameters')
    )
)

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /local/reps/CMSSW/CMSSW/Configuration/GenProduction/python/EightTeV/MinBias_TuneZ2star_8TeV_pythia6_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6-MinBias TuneZ2star at 8TeV')
)
