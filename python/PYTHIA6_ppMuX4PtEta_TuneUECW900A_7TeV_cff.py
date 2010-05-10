import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUECW900ASettings_cfi import *
                                 
source = cms.Source("EmptySource")
generator = cms.EDFilter("Pythia6GeneratorFilter",
    pythiaPylistVerbosity = cms.untracked.int32(0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    crossSection = cms.untracked.double(21150000000.0),
    filterEfficiency = cms.untracked.double(0.00096),
    maxEventsToPrint = cms.untracked.int32(0),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('MSEL=1           ! User defined processes', 
            'MSTJ(22)=4       ! Decay unstable particles in a cylinder', 
            'PARJ(73)=1500.   ! max. radius for MSTJ(22)=4', 
            'PARJ(74)=3000.   ! max. Z for MSTJ(22)=4', 
            'MDCY(C130,1)=1   ! decay k0-longs', 
            'MDCY(C211,1)=1   ! decay pions', 
            'MDCY(C321,1)=1   ! decay kaons',
	    'CKIN(3)=6        ! pthat min'),
        # This is a vector of ParameterSet names to be read, in this order
        parameterSets = cms.vstring('pythiaUESettings',
            'processParameters')
    )
)

mugenfilter = cms.EDFilter("MCSmartSingleParticleFilter",
    Status = cms.untracked.vint32(     1,    1),
    ParticleID = cms.untracked.vint32(13,  -13),
    MinPt = cms.untracked.vdouble(    4, 4),
    MaxEta = cms.untracked.vdouble(   2.1, 2.1),
    MinEta = cms.untracked.vdouble(  -2.1, -2.1),
    MaxDecayRadius = cms.untracked.vdouble(2000.0, 2000.0),
    MaxDecayZ = cms.untracked.vdouble(4000.0, 4000.0),
    MinDecayZ = cms.untracked.vdouble(-4000.0, -4000.0)
)

ProductionFilterSequence = cms.Sequence(generator*mugenfilter)

