import FWCore.ParameterSet.Config as cms

source = cms.Source("EmptySource")

from Configuration.Generator.PythiaUEZ2Settings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    comEnergy = cms.double(7000.0),
    crossSection = cms.untracked.double(7.069e-03),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('MSEL=0  !User selected',
            'MSTP(6)=1              ! excited quark', 
            'MSUB(147)=1            ! dg--> d*',
            'MSUB(148)=1            ! ug--> u*',
            'PMAS(343,1) = 2000.     ! d* mass',
            'PMAS(344,1) = 2000.     ! u* mass', 
            'RTCM(41) = 2000.        ! Scale parameter Lambda',
            #Below the couplings
            'RTCM(43)=0.5              ! f=1   SM coupling', 
            'RTCM(44)=0.5              ! fprime=1  SM coupling', 
            'RTCM(45)=0.5              ! f_s=1 SM coupling',
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
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/Excited_Quark_2000GeV_fhalf_cff.py,v $'),
    annotation = cms.untracked.string('qstar Mass = 2 TeV fhalf at 7 TeV')
)
