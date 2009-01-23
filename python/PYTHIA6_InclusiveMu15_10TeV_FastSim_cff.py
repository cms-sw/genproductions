import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
source = cms.Source("PythiaSource",
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(0.000239),
    comEnergy = cms.untracked.double(10000.0),
    crossSection = cms.untracked.double(509100000.),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring(
            'MSEL=1           ! User defined processes', 
            'CKIN(3)=20.      ! minimum pt hat for hard interactions',
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

## mugenfilter = cms.EDFilter("MCSmartSingleParticleFilter",
##                            MinPt = cms.untracked.vdouble(15.,15.),
##                            MinEta = cms.untracked.vdouble(-2.5,-2.5),
##                            MaxEta = cms.untracked.vdouble(2.5,2.5),
##                            ParticleID = cms.untracked.vint32(13,-13),
##                            Status = cms.untracked.vint32(1,1),
##                            # Decay cuts are in mm
##                            MaxDecayRadius = cms.untracked.vdouble(1500.,1500.),
##                            MinDecayZ = cms.untracked.vdouble(-3000.,-3000.),
##                            MaxDecayZ = cms.untracked.vdouble(3000.,3000.)
## )

mugenfilter = cms.EDFilter("MCDecayingPionKaonFilter",
                           PtMuMin = cms.untracked.double(15.0),
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
    version = cms.untracked.string('$Revision: 1.4 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_InclusiveMu15_10TeV_FastSim_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6-MinBias at 10TeV, pthat>20, with INCLUSIVE muon preselection (pt(mu) > 15) - FastSim Version')
)

ProductionFilterSequence = cms.Sequence(mugenfilter)
