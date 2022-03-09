import FWCore.ParameterSet.Config as cms
from Configuration.Generator.PythiaUEZ2Settings_cfi import *

generator = cms.EDFilter("Pythia6HadronizerFilter",
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(0.254),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(8000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    PythiaParameters = cms.PSet(
        pythiaUESettings = cms.vstring(
            'MSTJ(11)=3      ! Choice of the fragmentation function',
            'MSTP(81)=1      ! multiple parton interactions 1 is Pythia default',
            'MSTP(82)=4      ! Defines the multi-parton model',
            'PARP(67)=2.5    ! amount of initial-state radiation',
            'PARP(85)=1.0    ! gluon prod. mechanism in MI',
            'PARP(86)=1.0    ! gluon prod. mechanism in MI',
            'PARP(64)=0.2    ! ',
            'PARP(91)=2.1    ! kt distribution' 
        ),
        processParameters = cms.vstring(
            'MSEL=0          ! User defined processes',
            'PMAS(5,1)=4.4   ! b quark mass',
            'PMAS(6,1)=172.4 ! t quark mass',
            'MSTJ(1)=1       ! Fragmentation/hadronization on or off',
            'MSTP(61)=1      ! Parton showering on or off'),
        parameterSets = cms.vstring('pythiaUESettings', 'processParameters')
    )
)
