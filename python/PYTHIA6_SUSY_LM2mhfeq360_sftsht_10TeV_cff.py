import FWCore.ParameterSet.Config as cms

source = cms.Source("EmptySource")

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(1.0),
    crossSection = cms.untracked.double(2.047),
    comEnergy = cms.double(10000.0),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('MSEL=0         ! User defined processes', 
            'MSEL=39                  ! All SUSY processes',
            'IMSS(1) = 11             ! Spectrum from external SLHA file',
            'IMSS(21) = 33            ! LUN number for SLHA File (must be 33)',
            'IMSS(22) = 33            ! Read-in SLHA decay table'),
	SLHAParameters = cms.vstring(
	    'SLHAFILE = Configuration/Generator/data/SUSY_LM2mhfeq360_sftsht.slha'),
        # This is a vector of ParameterSet names to be read, in this order
        parameterSets = cms.vstring('pythiaUESettings', 
            'processParameters', 'SLHAParameters')
    )
)

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.11 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_SUSY_LM2mhfeq360_sftsht_10TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6_SUSY_LM2mhfeq360_sftsht at 10TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
