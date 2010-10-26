import FWCore.ParameterSet.Config as cms

from Configuration.Generator.PythiaUEZ2Settings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(42.2),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,processParameters = cms.vstring('MSEL=0             ! User defined processes', 
            "MSUB(141)   = 1    ! ff -> gamma/Z0/Z\'", 
            'MSTP(44)    = 3    ! no Zprime/Z/gamma interference', 
            "PMAS(32,1)  = 500 ! Z\' mass (GeV)", 
            'CKIN(1)     = -1  ! lower invariant mass cutoff (GeV)', 
            'CKIN(2)     = -1   ! no upper invariant mass cutoff', 
            'MDME(289,1) = 1    ! d dbar', 
            'MDME(290,1) = 1    ! u ubar', 
            'MDME(291,1) = 1    ! s sbar', 
            'MDME(292,1) = 1    ! c cbar', 
            'MDME(293,1) = 1    ! b bar', 
            'MDME(294,1) = 0    ! t tbar', 
            'MDME(295,1) = -1   ! 4th gen q qbar', 
            'MDME(296,1) = -1   ! 4th gen q qbar', 
            'MDME(297,1) = 0    ! e-     e+', 
            'MDME(298,1) = 0    ! nu_e   nu_ebar', 
            'MDME(299,1) = 0    ! mu-    mu+', 
            'MDME(300,1) = 0    ! nu_mu  nu_mubar', 
            'MDME(301,1) = 0    ! tau    tau', 
            'MDME(302,1) = 0    ! nu_tau nu_taubar', 
            'MDME(303,1) = -1   ! 4th gen l- l+', 
            'MDME(304,1) = -1   ! 4th gen nu nubar', 
            'MDME(305,1) = -1   ! W+ W-', 
            'MDME(306,1) = -1   ! H+ H-', 
            'MDME(307,1) = -1   ! Z0 gamma', 
            'MDME(308,1) = -1   ! Z0 h0', 
            'MDME(309,1) = -1   ! h0 A0', 
            'MDME(310,1) = -1   ! H0 A0'),
     
        parameterSets = cms.vstring('pythiaUESettings', 
            'processParameters')
    )
)

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_ZPrimeSSMDiJet_M500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA ZPRIME SSM di-jet, mass = 500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
