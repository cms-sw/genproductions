import FWCore.ParameterSet.Config as cms

configurationMetadata = cms.untracked.PSet(
        version = cms.untracked.string('$Revision: 1.1 $'),
        name = cms.untracked.string('$ $'),
        annotation = cms.untracked.string('Summer09: Pythia6-MinBias at 7TeV with Muon preselection (Loose cut), no relative muon charge requirement, +decay-inflight, 7TeV, D6T tune')
)

from Configuration.Generator.PythiaUEZ2Settings_cfi import *

generator = cms.EDFilter("Pythia6GeneratorFilter",
    pythiaPylistVerbosity = cms.untracked.int32(0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    crossSection = cms.untracked.double(48440000000.0),
    filterEfficiency = cms.untracked.double(0.00084),
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
            'CKIN(3)=2.   ! minimum allowed value for pt hat for hard interactions',
            'CKIN(13)=-5.  ! minimum allowed value for eta for hard interactions',
            'CKIN(14)=5.   ! maximum allowed value for eta for hard interactions',
            'CKIN(15)=-5.  ! minimum allowed value for eta for hard interactions',
            'CKIN(16)=5.   ! maximum allowed value for eta for hard interactions'
            ),
        # This is a vector of ParameterSet names to be read, in this order
        parameterSets = cms.vstring('pythiaUESettings',
            'processParameters')
    )
)

mumugenfilter = cms.EDFilter("MCParticlePairFilter",
    Status = cms.untracked.vint32(1, 1),
    # Change with respect to ppMuMuX config: from MinP to MinPt
    MinPt = cms.untracked.vdouble(2.5, 2.5),
    MaxEta = cms.untracked.vdouble(2.5, 2.5),
    MinEta = cms.untracked.vdouble(-2.5, -2.5),
    # Only change with respect to ppMuMuX config: ParticleCharge from -1 to 0
    ParticleCharge = cms.untracked.int32(0),
    ParticleID1 = cms.untracked.vint32(13),
    ParticleID2 = cms.untracked.vint32(13)
)

ProductionFilterSequence = cms.Sequence(generator*mumugenfilter)
