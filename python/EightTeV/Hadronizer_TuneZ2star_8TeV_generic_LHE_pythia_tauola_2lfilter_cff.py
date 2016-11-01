import FWCore.ParameterSet.Config as cms

from Configuration.Generator.PythiaUEZ2starSettings_cfi import *
from GeneratorInterface.ExternalDecays.TauolaSettings_cff import *

generator = cms.EDFilter("Pythia6HadronizerFilter",
    pythiaHepMCVerbosity = cms.untracked.bool(True),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    comEnergy = cms.double(8000.0),
    ExternalDecays = cms.PSet(
        Tauola = cms.untracked.PSet(
            TauolaPolar,
            TauolaDefaultInputCards
        ),
        parameterSets = cms.vstring('Tauola')
    ),
    UseExternalGenerators = cms.untracked.bool(True),                     
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('MSEL=0          ! User defined processes',
                                        'PMAS(5,1)=4.4   ! b quark mass',
                                        'PMAS(6,1)=172.4 ! t quark mass'),
        # This is a vector of ParameterSet names to be read, in this order
        parameterSets = cms.vstring('pythiaUESettings',
                                    'processParameters'
                                    )
        )
)


leplepgenfilter = cms.EDFilter("MCParticlePairFilter",
    Status = cms.untracked.vint32(1, 1),
    MinPt = cms.untracked.vdouble(7., 7.),
    MinEta = cms.untracked.vdouble(-3.0, -3.0),
    MaxEta = cms.untracked.vdouble(3.0, 3.0),
    ParticleCharge = cms.untracked.int32(0),
    ParticleID1 = cms.untracked.vint32(11,13),
    ParticleID2 = cms.untracked.vint32(11,13)
)


ProductionFilterSequence = cms.Sequence(generator*leplepgenfilter)

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string ('$Source: /cvs/CMSSW/CMSSW/Configuration/GenProduction/python/EightTeV/Hadronizer_TuneZ2star_8TeV_generic_LHE_pythia_cff.py,v $'),
    annotation = cms.untracked.string('runs Z2* Pythia6')
)

