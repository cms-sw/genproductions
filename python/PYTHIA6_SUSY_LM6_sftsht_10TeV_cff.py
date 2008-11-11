import FWCore.ParameterSet.Config as cms


from Configuration.GenProduction.PythiaUESettings_cfi import *
source = cms.Source("PythiaSource",
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(1.0),
    comEnergy = cms.untracked.double(10000.0),
    crossSection = cms.untracked.double(1.28),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring( 
            'MSEL=39                  ! All SUSY processes',
            'IMSS(1) = 11             ! Spectrum from external SLHA file',
            'IMSS(21) = 33            ! LUN number for SLHA File (must be 33)',
            'IMSS(22) = 33            ! Read-in SLHA decay table'),
	SLHAParameters = cms.vstring(
	    'SLHAFILE = Configuration/Generator/data/CSA07SUSYBSM_LM6_sftsdkpyt_slha.out'),
        # This is a vector of ParameterSet names to be read, in this order
        parameterSets = cms.vstring('pythiaUESettings', 
            'processParameters',
	    'SLHAParameters')
    )
)

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.8 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_MinBias_10TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6-SUSY-LM6-sftsht at 10TeV')
)

