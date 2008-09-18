import FWCore.ParameterSet.Config as cms


from Configuration.GenProduction.PythiaUESettings_cfi import *
source = cms.Source("PythiaSource",
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(1.0),
    comEnergy = cms.untracked.double(10000.0),
    crossSection = cms.untracked.double(0.00125),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('MSEL=0                    !(D=1) to select between full user control (0, then use MSUB) and some preprogrammed alternative', 
            'MSTP(6)=1                 ! excited quarks', 
            'MSUB(147)=1               ! dstar production', 
            'MSUB(148)=1               ! ustar production', 
            'PMAS(343,1)=5000.0        ! d* mass', 
            'PMAS(344,1)=5000.0        ! u* mass', 
            'RTCM(41)=5000.0           ! Lambda Scale equals qstar mass', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1. %'),
    annotation = cms.untracked.string('default documentation string for QstarDijet_5000_cff.py'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_Exotica_QstarDijet_5000_cff_py_GEN_IDEAL.py,v $')

)

