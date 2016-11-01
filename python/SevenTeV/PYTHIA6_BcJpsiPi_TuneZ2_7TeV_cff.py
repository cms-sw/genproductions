import FWCore.ParameterSet.Config as cms

from Configuration.Generator.PythiaUEZ2Settings_cfi import *

generator = cms.EDFilter(
    "Pythia6HadronizerFilter",
    maxEventsToPrint = cms.untracked.int32(10),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    comEnergy = cms.double(7000.),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('MSTP(51)=10042', 
            'MSTP(52)=2', 
            'MSTP(61)=0             ! Hadronization of the initial protons', 
            'MDME(997,2) = 0        ! PHASE SPACE', 
            'KFDP(997,1) = 211      ! pi+', 
            'KFDP(997,2) = 443      ! J/psi', 
            'KFDP(997,3) = 0        ! nada', 
            'KFDP(997,4) = 0        ! nada', 
            'KFDP(997,5) = 0        ! nada', 
            'PMAS(143,1) = 6.286', 
            'PMAS(143,4) = 0.138', 
            'MDME(858,1) = 0  ! J/psi->e+e-', 
            'MDME(859,1) = 1  ! J/psi->mumu', 
            'MDME(860,1) = 0', 
            'MDME(998,1) = 3', 
            'MDME(999,1) = 3', 
            'MDME(1000,1) = 3', 
            'MDME(1001,1) = 3', 
            'MDME(1002,1) = 3', 
            'MDME(1003,1) = 3', 
            'MDME(1004,1) = 3', 
            'MDME(1005,1) = 3', 
            'MDME(1006,1) = 3', 
            'MDME(1007,1) = 3', 
            'MDME(1008,1) = 3', 
            'MDME(1009,1) = 3', 
            'MDME(1010,1) = 3', 
            'MDME(1011,1) = 3', 
            'MDME(1012,1) = 3', 
            'MDME(1013,1) = 3', 
            'MDME(1014,1) = 3', 
            'MDME(1015,1) = 3', 
            'MDME(1016,1) = 3', 
            'MDME(1017,1) = 3', 
            'MDME(1018,1) = 3', 
            'MDME(1019,1) = 3', 
            'MDME(1020,1) = 3', 
            'MDME(1021,1) = 3', 
            'MDME(1022,1) = 3', 
            'MDME(1023,1) = 3', 
            'MDME(1024,1) = 3', 
            'MDME(1025,1) = 3', 
            'MDME(1026,1) = 3', 
            'MDME(1027,1) = 3', 
            'MDME(997,1) = 2        !  Bc -> pi J/Psi'),
        parameterSets = cms.vstring('pythiaUESettings', 
            'processParameters')
    )
)

bcfilter = cms.EDFilter("PythiaFilter",
    ParticleID = cms.untracked.int32(541)
)

ProductionFilterSequence = cms.Sequence(generator*bcfilter)
