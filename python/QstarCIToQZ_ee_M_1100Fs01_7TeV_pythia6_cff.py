import FWCore.ParameterSet.Config as cms

from Configuration.Generator.PythiaUEZ2Settings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring(
            'MSEL        = 0 ',
            'MSUB(147)   = 0     !d* ',
            'MSUB(148)   = 0     !u* ',
            'MSUB(167)   = 1     !u* ci ',
            'MSUB(168)   = 1     !d* ci ',
            'PMAS(343,1) = 1100  !mass of d*',
            'PMAS(344,1) = 1100  !mass of u*',
            'RTCM(41)    = 1100  !Lambda = mass ',
            'RTCM(43)    = 1     !f ',
            'RTCM(44)    = 1     !fp ',
            'RTCM(45)    = 0.1     !fs ',
            'MDME(174,1) = 0    !Z decay into d dbar',
            'MDME(175,1) = 0    !Z decay into u ubar',
            'MDME(176,1) = 0    !Z decay into s sbar',
            'MDME(177,1) = 0    !Z decay into c cbar',
            'MDME(178,1) = 0    !Z decay into b bbar',
            'MDME(179,1) = 0    !Z decay into t tbar',
            'MDME(182,1) = 1    !Z decay into e- e+',
            'MDME(183,1) = 0    !Z decay into nu_e nu_ebar',
            'MDME(184,1) = 0    !Z decay into mu- mu+',
            'MDME(185,1) = 0    !Z decay into nu_mu nu_mubar',
            'MDME(186,1) = 0    !Z decay into tau- tau+',
            'MDME(187,1) = 0    !Z decay into nu_tau nu_taubar',
            '4000001:ALLOFF            !Turn off all u* decays', 
            '4000001:ONIFMATCH 1 23    !Turn on u*->u Z', 
            '4000002:ALLOFF            !Turn off all d* decays', 
            '4000002:ONIFMATCH 2 23    !Turn on d*->d Z'
        ),
        parameterSets = cms.vstring('pythiaUESettings',
            'processParameters')
    )
)

#mumugenfilter = cms.EDFilter("MCParticlePairFilter",
#    Status = cms.untracked.vint32(1, 1),
#    MinPt = cms.untracked.vdouble(2.5, 2.5),
#    MaxEta = cms.untracked.vdouble(2.5, 2.5),
#    MinEta = cms.untracked.vdouble(-2.5, -2.5),
#    ParticleCharge = cms.untracked.int32(-1),
#    ParticleID1 = cms.untracked.vint32(13),
#    ParticleID2 = cms.untracked.vint32(13)
#)

#ProductionFilterSequence = cms.Sequence(generator*mumugenfilter)
ProductionFilterSequence = cms.Sequence(generator)

