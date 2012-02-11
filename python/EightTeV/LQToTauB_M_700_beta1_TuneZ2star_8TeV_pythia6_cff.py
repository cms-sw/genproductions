import FWCore.ParameterSet.Config as cms

source = cms.Source("EmptySource")

from Configuration.Generator.PythiaUEZ2starSettings_cfi import *
from GeneratorInterface.ExternalDecays.TauolaSettings_cff import *
generator = cms.EDFilter("Pythia6GeneratorFilter",
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(1.0),
    crossSection = cms.untracked.double(0.003478),
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
        processParameters = cms.vstring('MSEL=0            !User defined processes', 
                                        'PMAS(42,1)=700.0        ! LQ mass',
                             'IMSS(21)=33             ! LUN number for SLHA File (must be 33)',
                                        'IMSS(22)=33             ! Read-in SLHA decay table',
                                        'MSUB(163)=1             ! g+g->LQ+LQbar',
                                        'MSUB(164)=1             ! q+qbar->LQ+LQbar'),
        # This is a vector of ParameterSet names to be read, in this order
        parameterSets = cms.vstring('pythiaUESettings', 
                                    'processParameters',
                                    'SLHAParameters'),
       SLHAParameters = cms.vstring('SLHAFILE = Configuration/Generator/data/LQ_btau_beta1.0.out')
        )
)

ProductionFilterSequence = cms.Sequence(generator)
