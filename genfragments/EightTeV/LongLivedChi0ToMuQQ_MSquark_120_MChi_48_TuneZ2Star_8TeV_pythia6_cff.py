import FWCore.ParameterSet.Config as cms

from Configuration.Generator.PythiaUEZ2starSettings_cfi import *

source = cms.Source("EmptySource")
generator = cms.EDFilter("Pythia6GeneratorFilter", 
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    crossSection = cms.untracked.double(284.),
    comEnergy = cms.double(8000.0),
    UseExternalGenerators = cms.untracked.bool(False),
    PythiaParameters = cms.PSet(
            pythiaUESettingsBlock,
            pythiaParameters = cms.vstring(         
            'MSTJ(22)=1             ! Decay all unstable particles',
            'MSTP(95)=0            ! Disable colour reconnection, since it can put colour strings between widely separated partons', 
            'MSEL=0',
            'MSUB(271)=1            ! Squark pair production',
            'MSUB(272)=1',
            'MSUB(273)=1',
            'MSUB(274)=1',
            'MSUB(275)=1',
            'MSUB(276)=1',
            'MSUB(277)=1',
            'MSUB(278)=1',
            'MSUB(279)=1',
            'MSUB(280)=1',
            'IMSS(1)=1                ! General MSSM simultaion',

            'RMSS(2)=5000.                ! M2 mass',
            'RMSS(3)=5000.                ! M3 mass',
            'RMSS(4)=800.                 ! mu parameter',
            'RMSS(5)=2.                   ! tan Beta',
            'RMSS(6)=5000.                ! Left slepton mass',
            'RMSS(7)=5000.                ! Right slepton mass',
            'RMSS(10)=5000.               ! Left squark mass for third generation',
            'RMSS(11)=5000.               ! Right sbottom mass',
            'RMSS(12)=5000.               ! Right stop mass',
            'RMSS(13)=5000.               ! Left stau mass',
            'RMSS(14)=5000.               ! Right stau mass',
            
            'IMSS(52)=3               ! Turn on Lepton number violating LQD decay channels with all couplings set to zero',
            'RVLAMP(2,1,1)=0.000026   ! Set lambda Prime(2,1,1)',
            'MDME(2241,1)=0           ! Turn off LQD decays to neutrinos',
            'MDME(2242,1)=0           ! Turn off LQD decays to neutrinos',        
            'RMSS(1)=50              ! M1 mass',
            'RMSS(8)=120              ! Left squark mass',
            'RMSS(9)=120              ! Right squark mass'
        ),
        parameterSets = cms.vstring('pythiaUESettings',
            'pythiaParameters')
    )
)

ProductionFilterSequence = cms.Sequence(generator) 