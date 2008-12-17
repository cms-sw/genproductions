import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
source = cms.Source("PythiaSource",
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0000),
    comEnergy = cms.untracked.double(10000.0),
    crossSection = cms.untracked.double(51562800000.),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
	processParameters = cms.vstring(
             'MSEL=1   ! QCD hight pT processes',
             'CKIN(3)=0  ! minimum pt hat for hard interactions',
             'CKIN(4)=15  ! maximum pt hat for hard interactions'),
     # This is a vector of ParameterSet names to be read, in this order
        parameterSets = cms.vstring('pythiaUESettings', 
            'processParameters')

 
   


    )
)
configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.2 $'),
    name = cms.untracked.string
('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_QCDpt_0_15_10TeV_cff.py,v $'),
    annotation = cms.untracked.string('QCDpt-0-15 at 10TeV')
)

pthat_filter = cms.EDFilter("MCProcessFilter",
    MaxPthat = cms.untracked.vdouble(15., 15.0, 15.0, 15.0, 15.0,
        15.0),
    ProcessID = cms.untracked.vint32(11, 12, 13, 68, 28,
        53),
    MinPthat = cms.untracked.vdouble(0.0, 0.0, 0.0, 0.0, 0.0,
        0.0)
)
ProductionFilterSequence = cms.Sequence(pthat_filter)
