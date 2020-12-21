import FWCore.ParameterSet.Config as cms

source = cms.Source("EmptySource")

from Configuration.Generator.PythiaUEZ2starSettings_cfi import *
from GeneratorInterface.ExternalDecays.TauolaSettings_cff import *
generator = cms.EDFilter("Pythia6GeneratorFilter",
    pythiaPylistVerbosity = cms.untracked.int32(1),
    # put here the efficiency of your filter (1. if no filter)
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    # put here the cross section of your process (in pb)
    crossSection = cms.untracked.double(1.0),
    maxEventsToPrint = cms.untracked.int32(1),
    comEnergy = cms.double(8000.0),
    ExternalDecays = cms.PSet(
        Tauola = cms.untracked.PSet(
            TauolaPolar,
            TauolaDefaultInputCards
        ),
        parameterSets = cms.vstring('Tauola')
    ),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(25,1)=145       !mass of Higgs',
            'MSEL=0                  ! user selection for process',
            'MSUB(102)=0             !ggH',
            'MSUB(123)=0             !ZZ fusion to H',
            'MSUB(124)=0             !WW fusion to H',
            'MSUB(24)=1              !ZH production',
            'MSUB(26)=1              !WH production',
            'MSUB(121)=1             !gg to ttH',
            'MSUB(122)=1             !qq to ttH',

	    'MDME(174,1)=0           !Z decay into d dbar', 
            'MDME(175,1)=0           !Z decay into u ubar', 
            'MDME(176,1)=0           !Z decay into s sbar', 
            'MDME(177,1)=0           !Z decay into c cbar', 
            'MDME(178,1)=0           !Z decay into b bbar', 
            'MDME(179,1)=0           !Z decay into t tbar', 
            'MDME(182,1)=1           !Z decay into e- e+', 
            'MDME(183,1)=0           !Z decay into nu_e nu_ebar', 
            'MDME(184,1)=1           !Z decay into mu- mu+', 
            'MDME(185,1)=0           !Z decay into nu_mu nu_mubar', 
            'MDME(186,1)=1           !Z decay into tau- tau+', 
            'MDME(187,1)=0           !Z decay into nu_tau nu_taubar',
            'MDME(190,1) = 0            !W decay into dbar u',
            'MDME(191,1) = 0            !W decay into dbar c',
            'MDME(192,1) = 0            !W decay into dbar t',
            'MDME(194,1) = 0            !W decay into sbar u',
            'MDME(195,1) = 0            !W decay into sbar c',
            'MDME(196,1) = 0            !W decay into sbar t',
            'MDME(198,1) = 0            !W decay into bbar u',
            'MDME(199,1) = 0            !W decay into bbar c',
            'MDME(200,1) = 0            !W decay into bbar t',
            'MDME(206,1) = 1            !W decay into e+ nu_e',
            'MDME(207,1) = 1            !W decay into mu+ nu_mu',
            'MDME(208,1) = 1            !W decay into tau+ nu_tau',
            'MDME(210,1)=0           !Higgs decay into dd',
            'MDME(211,1)=0           !Higgs decay into uu',
            'MDME(212,1)=0           !Higgs decay into ss',
            'MDME(213,1)=0           !Higgs decay into cc',
            'MDME(214,1)=0           !Higgs decay into bb',
            'MDME(215,1)=0           !Higgs decay into tt',
            'MDME(216,1)=0           !Higgs decay into',
            'MDME(217,1)=0           !Higgs decay into Higgs decay',
            'MDME(218,1)=0           !Higgs decay into e nu e',
            'MDME(219,1)=0           !Higgs decay into mu nu mu',
            'MDME(220,1)=0           !Higgs decay into tau nu tau',
            'MDME(221,1)=0           !Higgs decay into Higgs decay',
            'MDME(222,1)=0           !Higgs decay into g g',
            'MDME(223,1)=0           !Higgs decay into gam gam',
            'MDME(224,1)=0           !Higgs decay into gam Z',
            'MDME(225,1)=0           !Higgs decay into Z Z',
            'MDME(226,1)=1           !Higgs decay into W W'
),

        # This is a vector of ParameterSet names to be read, in this order
        parameterSets = cms.vstring('pythiaUESettings',
            'processParameters')
    )
)

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /local/reps/CMSSW/CMSSW/Configuration/GenProduction/python/EightTeV/PYTHIA6_Tauola_SM_H_2W_2lnu_wh_mH145_lepdecay_8TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 WH, H->WW->lnulnu mH=145 with TAUOLA at 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
