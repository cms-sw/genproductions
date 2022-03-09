import FWCore.ParameterSet.Config as cms

pythiaStandardRPVBlock = cms.PSet(
        pythiaStandardRPVSettings = cms.vstring(
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
            'RMSS(14)=5000.               ! Right stau mass'
        )
)

