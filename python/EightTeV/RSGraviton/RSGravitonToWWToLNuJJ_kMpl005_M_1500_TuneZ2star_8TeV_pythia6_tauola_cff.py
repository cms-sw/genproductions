import FWCore.ParameterSet.Config as cms

source = cms.Source("EmptySource")

from Configuration.Generator.PythiaUEZ2starSettings_cfi import *
from GeneratorInterface.ExternalDecays.TauolaSettings_cff import *
generator = cms.EDFilter("Pythia6GeneratorFilter",
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(1.),
    comEnergy = cms.double(8000.0),
    crossSection = cms.untracked.double(0.003673),
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
        processParameters = cms.vstring('PMAS(5,1)= 4.8         ! b quark mass', 
                                        'PMAS(6,1)= 175.0       ! t quark mass', 
                                        'PMAS(347,1)= 1500.0     ! mass of RS Graviton', 
                                        'PARP(50) = 0.27        ! 0.054 == c=0.01 (k/M_PL=0.01)', 
                                        'MSEL=0                 ! (D=1) 0 to select full user control', 
                                        'MSUB(391)=1               ! q qbar -> G* ', 
                                        'MSUB(392)=1               ! g g -> G*', 
                                        '5000039:ALLOFF            ! Turn off all decays of G*', 
                                        '5000039:ONIFANY 24        ! Turn on the decays WW',
                                        
                                        'MDME(190,1)=    4   !W to d               ubar', 
                                        'MDME(191,1)=    4   !W to d               cbar', 
                                        'MDME(192,1)=    4   !W to d               tbar', 
                                        'MDME(194,1)=    4   !W to s               ubar', 
                                        'MDME(195,1)=    4   !W to s               cbar', 
                                        'MDME(196,1)=    4   !W to s               tbar', 
                                        'MDME(198,1)=    4   !W to b               ubar', 
                                        'MDME(199,1)=    4   !W to b               cbar', 
                                        'MDME(200,1)=    4   !W to b               tbar', 
                                        'MDME(206,1)=    5   !W to e               nu_e', 
                                        'MDME(207,1)=    5   !W to mu              nu_mu', 
                                        'MDME(208,1)=    5   !W to tau             nu_tau'), 
        # This is a vector of ParameterSet names to be read, in this order
        parameterSets = cms.vstring('pythiaUESettings', 
            'processParameters')
    )
)




ProductionFilterSequence = cms.Sequence(generator)
