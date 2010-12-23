import FWCore.ParameterSet.Config as cms

source = cms.Source("EmptySource")

from Configuration.Generator.PythiaUEZ2Settings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    comEnergy = cms.double(8000.0),
    crossSection = cms.untracked.double(0.4131),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('MSEL=0  !User selected',
            'MSTP(6)=1              ! excited quark', 
            'MSUB(147)=1            ! dg--> d*',
            'MSUB(148)=1            ! ug--> u*',
            'PMAS(343,1) = 1500.     ! d* mass',
            'PMAS(344,1) = 1500.     ! u* mass', 
            'RTCM(41) = 1500.        ! Scale parameter Lambda',
            #Below the couplings
            'RTCM(43)=1.0              ! f=1   SM coupling', 
            'RTCM(44)=1.0              ! fprime=1  SM coupling', 
            'RTCM(45)=1.0              ! f_s=1 SM coupling',
            #Below we decay them to photn+jet final state only...
            '4000001:ALLOFF            !Turn off all u* decays',
            '4000001:ONIFMATCH 1 22    !Turn on u*->u Photon',
            '4000002:ALLOFF            !Turn off all d* decays',
            '4000002:ONIFMATCH 2 22    !Turn on d*->d Photon'),
       # This is a vector of ParameterSet names to be read, in this order
        parameterSets = cms.vstring('pythiaUESettings', 
            'processParameters')
    )
)
ProductionFilterSequence = cms.Sequence(generator)
configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.3 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/QstarToGJ_M_1500_8TeV_pythia6_cff.py,v $'),
    annotation = cms.untracked.string('qstar Mass = 1.5 TeV at 8 TeV')
)

