import FWCore.ParameterSet.Config as cms

# Z2 tune with pT-ordered showers
from Configuration.Generator.PythiaUEZ2Settings_cfi import *

generator = cms.EDFilter(
    "Pythia6HadronizerFilter",
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    crossSection = cms.untracked.double(15.86),
    maxEventsToPrint = cms.untracked.int32(0),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock, 
        processParameters = cms.vstring(
            'MSEL=0           ! User defined processes', 
            'PMAS(5,1)=4.75   ! b quark mass', 
            'PMAS(6,1)=172.5  ! t quark mass' 
            ),
        parameterSets = cms.vstring(
            'pythiaUESettings', 
            'processParameters'
            )
        )
    )

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /local/projects/CMSSW/rep/CMSSW/Configuration/GenProduction/python/Attic/POWHEG_PYTHIA6_ttbar_lnublnub_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('POWHEG + PYTHIA6 - ttbar -> lnublnub at 7TeV')
    )

