import FWCore.ParameterSet.Config as cms

source = cms.Source("EmptySource")

from Configuration.Generator.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    pythiaPylistVerbosity = cms.untracked.int32(0),
    comEnergy = cms.double(10000.0),
    filterEfficiency = cms.untracked.double(1.0),
    maxEventsToPrint = cms.untracked.int32(0),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring(
            'MSEL=0           ! user defined processes',
            'MSUB(81)=1       ! qq to QQ massive',
            'MSUB(82)=1       ! gg to QQ massive',
            'MSTP(7)=5        ! 5 for bbbar',
            'CKIN(3)=15.      ! minimum pt hat for hard interactions', 
            'CKIN(4)=3000.    ! maximum pt hat for hard interactions',
            'MSTP(142)=2      ! Turns on the PYWEVT Pt reweighting routine' 
        ),
        CSAParameters = cms.vstring(
            'CSAMODE = 7     ! towards a "flat" QCD spectrum',
            'PTPOWER = 4.5   ! reweighting of the pt spectrum'
        ),
        parameterSets = cms.vstring(
            'pythiaUESettings', 
            'processParameters', 
            'CSAParameters'
        )
    )
)
ProductionFilterSequence = cms.Sequence(generator)
