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
    crossSection = cms.untracked.double(23.2),
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
                'MSUB(20)=1 !Wgamma production',
                'CKIN(3)=10.        !minimum pt hat for hard interactions',
    'MDME(190,1)=0      !W decay into dbar u',
    'MDME(191,1)=0      !W decay into dbar c',
    'MDME(192,1)=0      !W decay into dbar t',
    'MDME(194,1)=0      !W decay into sbar u',
    'MDME(195,1)=0      !W decay into sbar c',
    'MDME(196,1)=0      !W decay into sbar t',
    'MDME(198,1)=0      !W decay into bbar u',
    'MDME(199,1)=0      !W decay into bbar c',
    'MDME(200,1)=0      !W decay into bbar t',
    'MDME(206,1)=1      !W decay into e+ nu_e',
    'MDME(207,1)=1      !W decay into mu+ nu_mu',
    'MDME(208,1)=1      !W decay into tau+ nu_tau'),
        # This is a vector of ParameterSet names to be read, in this order
        parameterSets = cms.vstring('pythiaUESettings', 
            'processParameters')
    )
)

ProductionFilterSequence = cms.Sequence(generator)

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.3 $'),
    name = cms.untracked.string('$Source: /local/projects/CMSSW/rep/CMSSW/Configuration/GenProduction/python/Attic/WGtoLNuG_7TeV_pythia6_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6-EWK Wgamma at 7TeV')
)




