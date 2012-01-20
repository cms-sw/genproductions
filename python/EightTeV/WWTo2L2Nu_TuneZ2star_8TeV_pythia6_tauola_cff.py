import FWCore.ParameterSet.Config as cms

source = cms.Source("EmptySource")

from Configuration.Generator.PythiaUEZ2starSettings_cfi import *
from GeneratorInterface.ExternalDecays.TauolaSettings_cff import *
generator = cms.EDFilter("Pythia6GeneratorFilter",
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.),
    comEnergy = cms.double(8000.0),
    crossSection = cms.untracked.double(2.93),
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
        processParameters = cms.vstring('MSEL       =0      !User defined processes', 
                                        'MSUB(25)   =1      !WW production',
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
    version = cms.untracked.string('$Revision: 1.2 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/Attic/WWTo2L2Nu_TuneZ2_7TeV_pythia6_tauola_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6-EWK WW to 2l 2v at 8 TeV')
)


