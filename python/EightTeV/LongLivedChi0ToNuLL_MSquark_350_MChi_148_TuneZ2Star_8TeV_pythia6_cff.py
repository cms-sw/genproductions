import FWCore.ParameterSet.Config as cms

from Configuration.Generator.PythiaUEZ2starSettings_cfi import *

source = cms.Source("EmptySource")
generator = cms.EDFilter("Pythia6GeneratorFilter", 
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    crossSection = cms.untracked.double(3.851),
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
            
            'IMSS(51)=3               ! Turn on Lepton number violating LLE decay channels with all couplings set to zero',
            'RVLAM(1,2,2)=0.00065    ! Set lambda (1,2,2)',
            'RVLAM(1,2,1)=0.00065    ! Set lambda (2,1,1)',
            'RMSS(1)=150              ! M1 mass',
            'RMSS(8)=350              ! Left squark mass',
            'RMSS(9)=350              ! Right squark mass'
        ),
        parameterSets = cms.vstring('pythiaUESettings',
            'pythiaParameters')
    )
)

ProductionFilterSequence = cms.Sequence(generator) 