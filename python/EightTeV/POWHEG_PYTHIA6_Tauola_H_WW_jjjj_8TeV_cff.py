# -*- coding: utf-8 -*-
import FWCore.ParameterSet.Config as cms

# Z2star tune with pT-ordered showers
from Configuration.Generator.PythiaUEZ2starSettings_cfi import *
from GeneratorInterface.ExternalDecays.TauolaSettings_cff import *

generator = cms.EDFilter(
    "Pythia6HadronizerFilter",
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(8000.0),
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
            'MDME(225,1)=0    ! Higgs decay into Z Z',
            'MDME(226,1)=1    ! Higgs decay into W W',

            'MDME(190,1)=1    ! W decay into dbar u',
            'MDME(191,1)=1    ! W decay into dbar c',
            'MDME(192,1)=1    ! W decay into dbar t',
            'MDME(194,1)=1    ! W decay into sbar u',
            'MDME(195,1)=1    ! W decay into sbar c',
            'MDME(196,1)=1    ! W decay into sbar t',
            'MDME(198,1)=1    ! W decay into bbar u',
            'MDME(199,1)=1    ! W decay into bbar c',
            'MDME(200,1)=1    ! W decay into bbar t',
            'MDME(206,1)=0    ! W decay into e+ nu_e',
            'MDME(207,1)=0    ! W decay into mu+ nu_mu',
            'MDME(208,1)=0    ! W decay into tau+ nu_tau'
            ),
        parameterSets = cms.vstring(
            'pythiaUESettings', 
            'processParameters'
            )
        )
    )

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs/CMSSW/CMSSW/Configuration/GenProduction/python/EightTeV/POWHEG_PYTHIA6_Tauola_H_WW_jjjj_8TeV_cff.py,v $'),
    annotation = cms.untracked.string('POWHEG + PYTHIA6 + Tauola - Higgs -> WW -> jjjj at 8TeV')
    )
