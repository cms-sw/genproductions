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
            'MSEL=0       ', 
            'MSUB(14)=0   ', 
            'MSUB(18)=0   ', 
            'MSUB(19)=0   ', 
            'MSUB(20)=0   ', 
            'MSUB(29)=0   ', 
            'MSUB(11)=1   ', 
            'MSUB(12)=1   ', 
            'MSUB(13)=1   ', 
            'MSUB(15)=1   ', 
            'MSUB(16)=1   ', 
            'MSUB(28)=1   ', 
            'MSUB(30)=1   ', 
            'MSUB(31)=1   ', 
            'MSUB(53)=1   ', 
            'MSUB(68)=1   ', 
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

gj_filter = cms.EDFilter("PythiaFilterGammaJetWithBg",
                             MaxEvents = cms.untracked.int32(2),
                             MaxPhotonEta = cms.untracked.double(2.8),
                             MaxPhotonPt = cms.untracked.double(22.0),
                             MinPhotonEtaForwardJet = cms.untracked.double(1.3),
                             MinDeltaPhi = cms.untracked.double(170.0),
                             MinPhotonPt = cms.untracked.double(18.0),
                             MaxDeltaEta = cms.untracked.double(1.3),
                             PhotonSeedPt = cms.untracked.double(5.0)
                         )

ProductionFilterSequence = cms.Sequence(generator*gj_filter)
