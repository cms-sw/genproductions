import FWCore.ParameterSet.Config as cms

# Z2star tune with pT-ordered showers
from Configuration.Generator.PythiaUEZ2Settings_cfi import *
from GeneratorInterface.ExternalDecays.TauolaSettings_cff import *

generator = cms.EDFilter(
    "Pythia6HadronizerFilter",
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    ExternalDecays = cms.PSet(
        Tauola = cms.untracked.PSet(
            TauolaPolar,
            TauolaDefaultInputCards
            ),
        parameterSets = cms.vstring('Tauola')
        ),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock, 
        processParameters = cms.vstring(
            'MSEL=0           ! User defined processes', 
            'PMAS(5,1)=4.75   ! b quark mass', 
            'PMAS(6,1)=172.5  ! t quark mass',
            
            'MDME(210,1)=0    ! Higgs decay into dd',
            'MDME(211,1)=0    ! Higgs decay into uu',
            'MDME(212,1)=0    ! Higgs decay into ss',
            'MDME(213,1)=0    ! Higgs decay into cc',
            'MDME(214,1)=0    ! Higgs decay into bb',
            'MDME(215,1)=0    ! Higgs decay into tt',
            'MDME(216,1)=0    ! Higgs decay into bbbar prime',
            'MDME(217,1)=0    ! Higgs decay into ttbar prime',
            'MDME(218,1)=0    ! Higgs decay into e e',
            'MDME(219,1)=0    ! Higgs decay into mu mu',
            'MDME(220,1)=0    ! Higgs decay into tau tau',
            'MDME(221,1)=0    ! Higgs decay into tau tau prime',
            'MDME(222,1)=0    ! Higgs decay into g g',
            'MDME(223,1)=0    ! Higgs decay into gam gam',
            'MDME(224,1)=0    ! Higgs decay into gam Z',
            'MDME(225,1)=1    ! Higgs decay into Z Z',
            'MDME(226,1)=0    ! Higgs decay into W W',
            
            'MDME(174,1)=5           !Z decay into d dbar', 
            'MDME(175,1)=5           !Z decay into u ubar', 
            'MDME(176,1)=5           !Z decay into s sbar', 
            'MDME(177,1)=5           !Z decay into c cbar', 
            'MDME(178,1)=5           !Z decay into b bbar', 
            'MDME(179,1)=5           !Z decay into t tbar', 
            'MDME(182,1)=4           !Z decay into e- e+', 
            'MDME(183,1)=0           !Z decay into nu_e nu_ebar', 
            'MDME(184,1)=4           !Z decay into mu- mu+', 
            'MDME(185,1)=0           !Z decay into nu_mu nu_mubar', 
            'MDME(186,1)=4           !Z decay into tau- tau+', 
            'MDME(187,1)=0           !Z decay into nu_tau nu_taubar',
            
            'CKIN(45)=1.             !low mass cut on Z1',
            'CKIN(47)=1.             !low mass cut on Z2'  
            ),
        parameterSets = cms.vstring(
            'pythiaUESettings', 
            'processParameters'
            )
        )
    )

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.2 $'),
    name = cms.untracked.string('$Source: /cvs/CMSSW/CMSSW/Configuration/GenProduction/python/EightTeV/POWHEG_PYTHIA6_Tauola_H_ZZ_2l2q_8TeV_cff.py,v $'),
    annotation = cms.untracked.string('POWHEG + PYTHIA6 + Tauola - Higgs -> ZZ -> 2l2q at 8TeV')
    )
