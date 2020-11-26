import FWCore.ParameterSet.Config as cms

from Configuration.Generator.PythiaUEZ2starSettings_cfi import *

source = cms.Source("EmptySource")
generator = cms.EDFilter('Pythia6GeneratorFilter',
	comEnergy = cms.double(8000.0),
	crossSection = cms.untracked.double(1.0),
	filterEfficiency = cms.untracked.double(1.0),
	maxEventsToPrint = cms.untracked.int32(0),
	pythiaHepMCVerbosity = cms.untracked.bool(False),
	pythiaPylistVerbosity = cms.untracked.int32(0),
                         
        PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring(
            'PMAS(25,1)  = 120.0 ! mass of Higgs',
            'CKIN(3)     = 10.0  ! min pt hat for hard interaction gb to hb', 
            'MSEL        = 0     ! full user control', 
            'MSUB(32)    = 1     ! gq to hq, where q is assumed to be b',
            'MDME(210,1) = 0     ! h to dd',
            'MDME(211,1) = 0     ! h to uu',
            'MDME(212,1) = 0     ! h to ss',
            'MDME(213,1) = 0     ! h to cc',
            'MDME(214,1) = 1     ! h to bb',
            'MDME(215,1) = 0     ! h to tt',
            'MDME(216,1) = 0     ! h to bprime bprime',
            'MDME(217,1) = 0     ! h to tprime tprime',
            'MDME(218,1) = 0     ! h to ee',
            'MDME(219,1) = 0     ! h to mumu',
            'MDME(220,1) = 0     ! h to tautau',
            'MDME(221,1) = 0     ! h to tauprime tauprime',
            'MDME(222,1) = 0     ! h to gg',
            'MDME(223,1) = 0     ! h to gamma gamma',
            'MDME(224,1) = 0     ! h to gamma Z',
            'MDME(225,1) = 0     ! h to Z Z',
            'MDME(226,1) = 0     ! h to W W'),
        parameterSets = cms.vstring(
            'pythiaUESettings', 
            'processParameters'
        )
    )
)
