import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
source = cms.Source("PythiaSource",
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(0.000998),
    comEnergy = cms.untracked.double(10000.0),
    crossSection = cms.untracked.double(51600000000.),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('MSEL=0         ! User defined processes', 
            'MSUB(11)=1     ! Min bias process', 
            'MSUB(12)=1     ! Min bias process', 
            'MSUB(13)=1     ! Min bias process', 
            'MSUB(28)=1     ! Min bias process', 
            'MSUB(53)=1     ! Min bias process', 
            'MSUB(68)=1     ! Min bias process',
            'MSUB(95)=1     ! Min bias process'),
        # This is a vector of ParameterSet names to be read, in this order
        parameterSets = cms.vstring('pythiaUESettings', 
            'processParameters')
    )
)

elegenfilter = cms.EDFilter("MCSingleParticleFilter",
                             MinPt = cms.untracked.vdouble(2.5,2.5),
                             MinEta = cms.untracked.vdouble(-2.7,-2.7),
                             MaxEta = cms.untracked.vdouble(2.7,2.7),
                             ParticleID = cms.untracked.vint32(11,-11)
)

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/Attic/PYTHIA6_ppEleX_10TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6-MinBias at 10TeV with Electron preselection (pt > 2.5)')
)

ProductionFilterSequence = cms.Sequence(elegenfilter)
