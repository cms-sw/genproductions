

import FWCore.ParameterSet.Config as cms

source = cms.Source("EmptySource")

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0000),
    comEnergy = cms.double(900.0),
    crossSection = cms.untracked.double(18220000.),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
	processParameters = cms.vstring(
             'MSEL=1   ! QCD hight pT processes',
             'CKIN(3)=15  ! minimum pt hat for hard interactions',
             'CKIN(4)=20  ! maximum pt hat for hard interactions'),
     # This is a vector of ParameterSet names to be read, in this order
        parameterSets = cms.vstring('pythiaUESettings', 
            'processParameters')

 
   


    )
)
configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.4 $'),
    name = cms.untracked.string
('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_QCDpt_15_20_900GeV_cff.py,v $'),
    annotation = cms.untracked.string('QCDpt-15-20 at 900GeV')
)

ProductionFilterSequence = cms.Sequence(generator)
