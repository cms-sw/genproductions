import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *

source = cms.Source("EmptySource")
generator = cms.EDFilter("Pythia6GeneratorFilter",
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0000),
    comEnergy = cms.double(7000.0),
    crossSection = cms.untracked.double(3.412),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('MSEL=0    !(D=1) to select between full user control (0, then use MSUB) and some preprogrammed alternative',
	    'ITCM(5)=2      ! Switch on contact inteactions for all quarks',
	    'RTCM(41)=3000  ! Set Contact Scale Lambda to 3 TeV',
	    'RTCM(42)=1     ! Sign of contact interaction is +',
	    'MSUB(381)=1    ! qi qj -> qi qj via QCD plus a contact interaction',
	    'MSUB(382)=1    ! qi qiBar -> qk qkBar via QCD plus a contact interaction',
	    'MSUB(13)=1     ! qi qiBar -> g g via normal QCD',
	    'MSUB(28)=1     ! qi g -> qi g  via normal QCD',
	    'MSUB(53)=1     ! g g -> qk qkbar via normal QCD',
            'MSUB(68)=1     ! g g -> g g via normal QCD',
            'CKIN(3)=1000  ! minimum pt hat for hard interactions', 
            'CKIN(4)=1400  ! maximum pt hat for hard interactions'),
        parameterSets = cms.vstring('pythiaUESettings', 
            'processParameters')
    )
)

ProductionFilterSequence = cms.Sequence(generator)

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string
('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/Attic/PYTHIA6_QCDplus3TeVcontact_pt_1000_1400_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('QCD+3TeVcontact-pt-1000-1400 at 7TeV')
)

