import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
source = cms.Source("PythiaSource",
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(0.08),
    comEnergy = cms.untracked.double(10000.0),
    crossSection = cms.untracked.double(100000.),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring(
            'MSEL=1           ! User defined processes', 
            'CKIN(3)=150.      ! minimum pt hat for hard interactions',
            'MSTJ(22)=4       ! Decay unstable particles inside a cylinder',
            'PARJ(73)=10.   ! max. radius for MSTJ(22)=4',
            'PARJ(74)=3000.   ! max. Z for MSTJ(22)=4', 
            'MDCY(C130,1)=1   ! decay k0-longs',
            'MDCY(C211,1)=1   ! decay pions',
            'MDCY(C321,1)=1   ! decay kaons'),

        # This is a vector of ParameterSet names to be read, in this order
        parameterSets = cms.vstring('pythiaUESettings', 
            'processParameters')
    )
)

mugenfilter = cms.EDFilter("MCDecayingPionKaonFilter",
                           PtMuMin = cms.untracked.double(5.0),
                           ParticleID = cms.untracked.vint32(0, 211, -211, 321, -321, 130),
                           MinDecayRadius = cms.untracked.vdouble(0.0, 10.0, 10.0, 10.0, 10.0, 10.0),
                           MaxDecayRadius = cms.untracked.vdouble(10.0, 1500.0, 1500.0, 1500.0, 1500.0, 1500.0),
                           MinPt = cms.untracked.vdouble(5.0, 5.0, 5.0, 5.0, 5.0, 5.0),
                           MinEta = cms.untracked.vdouble(-2.5 ,-2.5, -2.5, -2.5, -2.5, -2.5),
                           MaxEta = cms.untracked.vdouble(2.5 ,2.5, 2.5, 2.5, 2.5, 2.5),
                           MinDecayZ = cms.untracked.vdouble(-3000.0, -3000.0, -3000.0, -3000.0, -3000.0, -3000.0),
                           MaxDecayZ = cms.untracked.vdouble(3000.0, 3000.0, 3000.0, 3000.0, 3000.0, 3000.0)
                           )


configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.2 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_InclusiveMu5_pt150_10TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6-MinBias at 10TeV, pthat>150, with INCLUSIVE muon preselection (pt(mu) > 5) - FastSim version')
)

ProductionFilterSequence = cms.Sequence(mugenfilter)
