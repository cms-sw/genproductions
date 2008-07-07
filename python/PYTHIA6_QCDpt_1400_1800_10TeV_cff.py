import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
source = cms.Source("PythiaSource",
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0000),
    comEnergy = cms.untracked.double(10000.0),
    crossSection = cms.untracked.double(0000.95690),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
	processParameters = cms.vstring(
             'MSEL=1   ! QCD hight pT processes',
             'CKIN(3)=1400  ! minimum pt hat for hard interactions',
             'CKIN(4)=1800  ! maximum pt hat for hard interactions'),
     # This is a vector of ParameterSet names to be read, in this order
        parameterSets = cms.vstring('pythiaUESettings', 
            'processParameters')

 
   


    )
)
configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string
('$Source: /local/projects/CMSSW/rep/CMSSW/Configuration/GenProduction/python/PYTHIA6_QCDpt_1400_1800_10TeV_cff.py,v $'),
    annotation = cms.untracked.string('QCDpt-1400-1800 at 10TeV')
)

