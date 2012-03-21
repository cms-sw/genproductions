import FWCore.ParameterSet.Config as cms

# Z2 star tune with pT-ordered showers
from Configuration.Generator.PythiaUEZ2starSettings_cfi import *
from GeneratorInterface.ExternalDecays.TauolaSettings_cff import *

generator = cms.EDFilter(
    "Pythia6HadronizerFilter",
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(8000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    ExternalDecays = cms.PSet(
        Tauola = cms.untracked.PSet(
            TauolaPolar,
            TauolaDefaultInputCards
            ),
        parameterSets = cms.vstring('Tauola')
        ),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock, 
        processParameters = cms.vstring(
            'MSEL=0           ! User defined processes', 
            'PMAS(5,1)=4.75   ! b quark mass', 
            'PMAS(6,1)=172.5  ! t quark mass' 
             
            'CKIN(41)=4.             !low mass cut on Z1',
            'CKIN(43)=4.             !low mass cut on Z2'
            ),
        parameterSets = cms.vstring(
            'pythiaUESettings', 
            'processParameters'
            )
        )
    )

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs/CMSSW/CMSSW/Configuration/GenProduction/python/EightTeV/POWHEG_PYTHIA6_Tauola_H_ZZ_4l_8TeV_cff.py,v $'),
    annotation = cms.untracked.string('POWHEG + PYTHIA6 + Tauola - Higgs -> ZZ -> 4l at 8TeV')
    )
