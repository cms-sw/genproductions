import FWCore.ParameterSet.Config as cms

source = cms.Source("EmptySource")

from Configuration.Generator.PythiaUEZ2Settings_cfi import *
from GeneratorInterface.ExternalDecays.TauolaSettings_cff import *

generator = cms.EDFilter("Pythia6GeneratorFilter",
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.),
    comEnergy = cms.double(7000.0),
    crossSection = cms.untracked.double(28),
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
        processParameters = cms.vstring('MSEL        = 0    !User defined processes', 
                                        'MSUB(25) = 1 !WW production'),
        # This is a vector of ParameterSet names to be read, in this order
        parameterSets = cms.vstring('pythiaUESettings', 
            'processParameters')
    )
)

ProductionFilterSequence = cms.Sequence(generator)

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.3 $'),
    name = cms.untracked.string('$Source: /local/projects/CMSSW/rep/CMSSW/Configuration/GenProduction/python/Attic/WWtoAnything_7TeV_pythia6_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6-EWK WW at 7TeV')
)

