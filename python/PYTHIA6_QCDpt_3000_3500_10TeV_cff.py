

import FWCore.ParameterSet.Config as cms

source = cms.Source("EmptySource")

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0000),
    comEnergy = cms.double(10000.0),
    crossSection = cms.untracked.double(00.0000084318),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
	processParameters = cms.vstring(
             'MSEL=1   ! QCD hight pT processes',
             'CKIN(3)=3000  ! minimum pt hat for hard interactions',
             'CKIN(4)=3500  ! maximum pt hat for hard interactions'),
     # This is a vector of ParameterSet names to be read, in this order
        parameterSets = cms.vstring('pythiaUESettings', 
            'processParameters')

 
   


    )
)
configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string
('$Source: /local/projects/CMSSW/rep/CMSSW/Configuration/GenProduction/python/PYTHIA6_QCDpt_3000_3500_10TeV_cff.py,v $'),
    annotation = cms.untracked.string('QCDpt-3000-3500 at 10TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
