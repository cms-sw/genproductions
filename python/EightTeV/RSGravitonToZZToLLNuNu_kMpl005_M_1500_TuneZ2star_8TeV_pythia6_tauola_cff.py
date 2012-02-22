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
    crossSection = cms.untracked.double(0.0001677),
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
                                        '5000039:ONIFANY 23        ! Turn on the decays ZZ',
                                        
                                        'MDME(174,1)=    0   !Z to d               dbar', 
                                        'MDME(175,1)=    0   !Z to u               ubar', 
                                        'MDME(176,1)=    0   !Z to s               sbar', 
                                        'MDME(177,1)=    0   !Z to c               cbar', 
                                        'MDME(178,1)=    0   !Z to b               bbar', 
                                        'MDME(179,1)=    0   !Z to t               tbar', 
                                        'MDME(182,1)=    4   !Z to e-              e+', 
                                        'MDME(183,1)=    5   !Z to nu_e            nu_ebar', 
                                        'MDME(184,1)=    4   !Z to mu-             mu+', 
                                        'MDME(185,1)=    5   !Z to nu_mu           nu_mubar', 
                                        'MDME(186,1)=    4   !Z to tau-            tau+', 
                                        'MDME(187,1)=    5   !Z to nu_tau          nu_taubar'), 
        # This is a vector of ParameterSet names to be read, in this order
        parameterSets = cms.vstring('pythiaUESettings', 
            'processParameters')
    )
)




ProductionFilterSequence = cms.Sequence(generator)
