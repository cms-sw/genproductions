import FWCore.ParameterSet.Config as cms

from Configuration.Generator.PythiaUEZ2Settings_cfi import *
from GeneratorInterface.ExternalDecays.TauolaSettings_cff import *

generator = cms.EDFilter(
    "Pythia6HadronizerFilter",
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    maxEventsToPrint = cms.untracked.int32(0),
    ExternalDecays = cms.PSet(
        Tauola = cms.untracked.PSet(
            TauolaPolar,
            TauolaDefaultInputCards
            ),
        parameterSets = cms.vstring('Tauola')
        ),
    comEnergy = cms.double(7000.0),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring(
            'MSEL      = 0     !User defined processes', 
            'PMAS(5,1) = 4.75  ! b quark mass',
            'PMAS(6,1) = 172.5 ! t quark mass'),
    # This is a vector of ParameterSet names to be read, in this order
        parameterSets = cms.vstring('pythiaUESettings', 
                                    'processParameters')
        )
)

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs/CMSSW/CMSSW/Configuration/GenProduction/python/POWHEG_PYTHIA6_Tauola_ZZ_TuneZ2_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('POWHEG + PYTHIA6+Tauola -> ZZ -> XX at 7TeV')
    )
