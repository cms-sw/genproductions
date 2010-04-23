import FWCore.ParameterSet.Config as cms

configurationMetadata = cms.untracked.PSet(
        version = cms.untracked.string('$Revision: 1.2 $'),
        name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_ppMuEl15X_pthat9_7TeV_cff.py,v $'),
        annotation = cms.untracked.string('Summer09: Pythia6-MinBias at 7TeV with Muon preselection , +decay-inflight, 7TeV, D6T tune')
)

from Configuration.GenProduction.PythiaUESettings_cfi import *

source = cms.Source("EmptySource")
generator = cms.EDFilter("Pythia6GeneratorFilter",
    pythiaPylistVerbosity = cms.untracked.int32(0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    crossSection = cms.untracked.double(5447000000.0),
    filterEfficiency = cms.untracked.double(0.000038),
    maxEventsToPrint = cms.untracked.int32(0),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('MSEL=1           ! User defined processes', 
            'MSTJ(22)=4       ! Decay unstable particles in a cylinder', 
            'PARJ(73)=1500.   ! max. radius for MSTJ(22)=4', 
            'PARJ(74)=8000.   ! max. Z for MSTJ(22)=4', 
            'MDCY(C130,1)=1   ! decay k0-longs', 
            'MDCY(C211,1)=1   ! decay pions', 
            'MDCY(C321,1)=1   ! decay kaons',
            'CKIN(3)=9        ! pthat min'),
        # This is a vector of ParameterSet names to be read, in this order
        parameterSets = cms.vstring('pythiaUESettings',
            'processParameters')
    )
)


process.muelgenfilter = cms.EDFilter("MCSmartSingleParticleFilter",
    MaxDecayRadius = cms.untracked.vdouble(2000.0, 2000.0, 1.e5, 1.e5),
    Status = cms.untracked.vint32(1, 1, 1, 1),
    MinPt = cms.untracked.vdouble(15, 15, 15, 15),
    ParticleID = cms.untracked.vint32(13, -13, 11, -11),
    MaxEta = cms.untracked.vdouble(2.5, 2.5, 2.5, 2.5),
    MinEta = cms.untracked.vdouble(-2.5, -2.5, -2.5, -2.5),
    MaxDecayZ = cms.untracked.vdouble(9000.0, 9000.0, 1.e5, 1.e5),
    MinDecayZ = cms.untracked.vdouble(-9000.0, -9000.0, -1.e5, -1.e5)
)

ProductionFilterSequence = cms.Sequence(generator*muelgenfilter)
