import FWCore.ParameterSet.Config as cms

# Z2star tune with pT-ordered showers
from Configuration.Generator.PythiaUEZ2starSettings_cfi import *
from GeneratorInterface.ExternalDecays.TauolaSettings_cff import *

generator = cms.EDFilter(
    "Pythia6HadronizerFilter",
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency      = cms.untracked.double(1.0),
    pythiaHepMCVerbosity  = cms.untracked.bool(False),
    comEnergy             = cms.double(8000.0),
#    crossSection = cms.untracked.double(1.0),
    maxEventsToPrint      = cms.untracked.int32(0),
    ExternalDecays        = cms.PSet(
        Tauola            = cms.untracked.PSet(
            TauolaPolar,
            TauolaDefaultInputCards
        ),
        parameterSets     = cms.vstring('Tauola')
    ),
    PythiaParameters      = cms.PSet(
        pythiaUESettingsBlock, 
        processParameters = cms.vstring(
            'MSEL=0           ! User defined processes', 
            'PMAS(5,1)=4.75   ! b quark mass', 
            'PMAS(6,1)=172.5  ! t quark mass' 
            ),
        parameterSets     = cms.vstring(
            'pythiaUESettings', 
            'processParameters'
            )
        )
    )

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /local/reps/CMSSW/CMSSW/Configuration/GenProduction/python/EightTeV/POWHEG_PYTHIA6_top_tauola_cff.py,v $'),
    annotation = cms.untracked.string('POWHEG + PYTHIA6 Tune Z2star + Tauola at 7TeV')
    )


ttgenfilter = cms.EDFilter("MCParticlePairFilter",
    Status = cms.untracked.vint32(3, 3),
    MinPt = cms.untracked.vdouble(0., 0.),
    ParticleCharge = cms.untracked.int32(0),
    ParticleID1 = cms.untracked.vint32(6),
    ParticleID2 = cms.untracked.vint32(6),
    MinInvMass =  cms.untracked.double(700.),
    MaxInvMass =  cms.untracked.double(1000.)                       
)

ProductionFilterSequence = cms.Sequence(generator*ttgenfilter)
