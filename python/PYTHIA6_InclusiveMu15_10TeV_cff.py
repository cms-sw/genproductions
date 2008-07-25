import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
source = cms.Source("PythiaSource",
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(0.000289),
    comEnergy = cms.untracked.double(10000.0),
    crossSection = cms.untracked.double(509100000.),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring(
            'MSEL=1           ! User defined processes', 
            'CKIN(3)=20.      ! minimum pt hat for hard interactions',
            'PARJ(71)=20000.  ! max. proper lifetime time ctau in mm',
            'MDCY(C130,1)=1   ! decay k0-longs',
            'MDCY(C211,1)=1   ! decay pions',
            'MDCY(C321,1)=1   ! decay kaons'),

        # This is a vector of ParameterSet names to be read, in this order
        parameterSets = cms.vstring('pythiaUESettings', 
            'processParameters')
    )
)

mugenfilter = cms.EDFilter("MCSmartSingleParticleFilter",
                           MinPt = cms.untracked.vdouble(15.,15.),
                           MinEta = cms.untracked.vdouble(-2.5,-2.5),
                           MaxEta = cms.untracked.vdouble(2.5,2.5),
                           ParticleID = cms.untracked.vint32(13,-13),
                           Status = cms.untracked.vint32(1,1),
                           # Decay cuts are in mm
                           MaxDecayRadius = cms.untracked.vdouble(1500.,1500.),
                           MinDecayZ = cms.untracked.vdouble(-3000.,-3000.),
                           MaxDecayZ = cms.untracked.vdouble(3000.,3000.)
)


configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /local/projects/CMSSW/rep/CMSSW/Configuration/GenProduction/python/PYTHIA6_InclusiveMu15_10TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6-MinBias at 10TeV, pthat>20, with INCLUSIVE muon preselection (pt(mu) > 15)')
)

ProductionFilterSequence = cms.Sequence(mugenfilter)
