import FWCore.ParameterSet.Config as cms

from Configuration.Generator.PythiaUEZ2starSettings_cfi import *

source = cms.Source("EmptySource")
generator = cms.EDFilter("Pythia6GeneratorFilter", 
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(1.0),
    comEnergy = cms.double(8000.0),
    crossSection = cms.untracked.double(1195.0),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('MSEL=0                    !(D=1) to select between full user control (0, then use MSUB) and some preprogrammed alternative', 
            'MSTP(6)=1                 ! excited quarks', 
            'MSUB(147)=1               ! dstar production', 
            'MSUB(148)=1               ! ustar production', 
            'PMAS(343,1)=700.0         ! d* mass', 
            'PMAS(344,1)=700.0         ! u* mass', 
            'RTCM(41)=700.0            ! Lambda Scale equals qstar mass', 
            'RTCM(43)=1.0              ! f=1   SM coupling', 
            'RTCM(44)=1.0              ! fprime=1  SM coupling', 
            'RTCM(45)=1.0              ! f_s=1 SM coupling', 
	    '4000001:ALLOFF            !Turn off all u* decays',
            '4000001:ONIFMATCH 1 21    !Turn on u*->u g',
	    '4000002:ALLOFF            !Turn off all d* decays',
            '4000002:ONIFMATCH 2 21    !Turn on d*->d g'),
        parameterSets = cms.vstring('pythiaUESettings', 
            'processParameters')
    )
)


ProductionFilterSequence = cms.Sequence(generator) 


