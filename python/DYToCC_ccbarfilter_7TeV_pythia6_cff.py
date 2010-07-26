import FWCore.ParameterSet.Config as cms

source = cms.Source("EmptySource")

from Configuration.Generator.PythiaUESettings_cfi import *

generator = cms.EDFilter(
    "Pythia6GeneratorFilter",
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(0.6990),
    comEnergy = cms.double(7000.0),
    crossSection = cms.untracked.double(2588.0),
    PythiaParameters = cms.PSet(
    pythiaUESettingsBlock,
    processParameters = cms.vstring(
    'MSEL=0      ',
    'MSUB(1)=1   ',
    'MSTP(43)=3     !both Z0 and gamma*',
    
    'MDME(174,1)=0  !dd~',
    'MDME(175,1)=0  !uu~ ',
    'MDME(176,1)=0  !ss~',
    'MDME(177,1)=1  !cc~ ',
    'MDME(178,1)=0  !bb~ ', 
    'MDME(179,1)=0  !tt~',
    'MDME(182,1)=0  !ee',
    'MDME(183,1)=0  !nunu', 
    'MDME(184,1)=0  !mumu',
    'MDME(185,1)=0  !nunu',
    'MDME(186,1)=0  !tautau', 
    'MDME(187,1)=0  !nunu ',
    
    'CKIN(1)=50.    !Minimum sqrt(s_hat) value (=Z mass)'
    ),
    
    # This is a vector of ParameterSet names to be read, in this order
    
    parameterSets = cms.vstring('pythiaUESettings', 
                                'processParameters')
    
    
    
    )
    )

ccbargenfilter = cms.EDFilter("MCParticlePairFilter",
                              Status = cms.untracked.vint32(3, 3),
                              MinPt = cms.untracked.vdouble(1.0, 1.0),
                              MaxEta = cms.untracked.vdouble(3.0, 3.0),
                              MinEta = cms.untracked.vdouble(-3.0, -3.0),
                              ParticleCharge = cms.untracked.int32(-1),
                              ParticleID1 = cms.untracked.vint32(4),
                              ParticleID2 = cms.untracked.vint32(4)
                              )

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string
    ('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_Zbb_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('Z -> c cbar at 7TeV')
    )

ProductionFilterSequence = cms.Sequence(generator*ccbargenfilter)
