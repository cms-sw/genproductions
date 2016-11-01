import FWCore.ParameterSet.Config as cms

source = cms.Source("EmptySource")

from Configuration.Generator.PythiaUEZ2starSettings_cfi import *
from GeneratorInterface.ExternalDecays.TauolaSettings_cff import *

generator = cms.EDFilter("Pythia6GeneratorFilter",
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.),
    comEnergy = cms.double(14000.0),
    crossSection = cms.untracked.double(1.),
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
                                        'MSUB(22) = 1 !ZZ production',

                                        'MDME(174,1)=5            !Z decay into d dbar',
                                        'MDME(175,1)=5            !Z decay into u ubar',
                                        'MDME(176,1)=5            !Z decay into s sbar',
                                        'MDME(177,1)=5            !Z decay into c cbar',
                                        'MDME(178,1)=5            !Z decay into b bbar',
                                        'MDME(179,1)=5            !Z decay into t tbar',
                                        'MDME(182,1)=4            !Z decay into e- e+',
                                        'MDME(183,1)=5            !Z decay into nu_e nu_ebar',
                                        'MDME(184,1)=4            !Z decay into mu- mu+',
                                        'MDME(185,1)=5            !Z decay into nu_mu nu_mubar',
                                        'MDME(186,1)=4            !Z decay into tau- tau+',
                                        'MDME(187,1)=5            !Z decay into nu_tau nu_taubar',
                                        
                                        'CKIN(41)=75.             !low mass cut on Z1',
                                        ),
        # This is a vector of ParameterSet names to be read, in this order
        parameterSets = cms.vstring('pythiaUESettings', 
            'processParameters')
    )
)

ProductionFilterSequence = cms.Sequence(generator)
