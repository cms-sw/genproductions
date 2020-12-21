
import FWCore.ParameterSet.Config as cms

source = cms.Source("EmptySource")

from Configuration.Generator.PythiaUEZ2starSettings_cfi import *
from GeneratorInterface.ExternalDecays.TauolaSettings_cff import *

generator = cms.EDFilter("Pythia6GeneratorFilter",
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(1.),
    crossSection = cms.untracked.double(0.001525),
    ExternalDecays = cms.PSet(
        Tauola = cms.untracked.PSet(
            TauolaPolar,
            TauolaDefaultInputCards
        ),
        parameterSets = cms.vstring('Tauola')
    ),
    UseExternalGenerators = cms.untracked.bool(True),        
    comEnergy = cms.double(8000.0),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('MSEL        = 0    !User defined processes', 
                                        'MSUB(2)   = 1    !W production',                                       
                                        'MDME(190,1) = 0    !W decay into dbar u', 
                                        'MDME(191,1) = 0    !W decay into dbar c', 
                                        'MDME(192,1) = 0    !W decay into dbar t', 
                                        'MDME(194,1) = 0    !W decay into sbar u', 
                                        'MDME(195,1) = 0    !W decay into sbar c', 
                                        'MDME(196,1) = 0    !W decay into sbar t', 
                                        'MDME(198,1) = 0    !W decay into bbar u', 
                                        'MDME(199,1) = 0    !W decay into bbar c', 
                                        'MDME(200,1) = 0    !W decay into bbar t', 
                                        'MDME(205,1) = 0    !W decay into bbar tp', 
                                        'MDME(206,1) = 0    !W decay into e+ nu_e', 
                                        'MDME(207,1) = 0    !W decay into mu+ nu_mu', 
                                        'MDME(208,1) = 1    !W decay into tau+ nu_tau',
		                        'CKIN(3)=500.       ! min pt hat value'),
        # This is a vector of ParameterSet names to be read, in this order
        parameterSets = cms.vstring('pythiaUESettings', 
            'processParameters')
    )
)

ProductionFilterSequence = cms.Sequence(generator)

