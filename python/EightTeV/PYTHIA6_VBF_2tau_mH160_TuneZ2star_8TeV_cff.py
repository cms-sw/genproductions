import FWCore.ParameterSet.Config as cms

from Configuration.Generator.PythiaUEZ2starSettings_cfi import *

source = cms.Source("EmptySource")
generator = cms.EDFilter('Pythia6GeneratorFilter',
	comEnergy = cms.double(8000.0),
	crossSection = cms.untracked.double(1.0),
	filterEfficiency = cms.untracked.double(1.0),
	maxEventsToPrint = cms.untracked.int32(0),
	pythiaHepMCVerbosity = cms.untracked.bool(False),
	pythiaPylistVerbosity = cms.untracked.int32(0),

	PythiaParameters = cms.PSet(
		pythiaUESettingsBlock,
		processParameters = cms.vstring('PMAS(25,1)=160.0        !mass of Higgs', 
            'MSEL=0                  ! user selection for process', 
            'MSUB(102)=0             !ggH', 
            'MSUB(123)=1             !ZZ fusion to H', 
            'MSUB(124)=1             !WW fusion to H', 
            'MDME(190,1) = 0            !W decay into dbar u', 
            'MDME(191,1) = 0            !W decay into dbar c', 
            'MDME(192,1) = 0            !W decay into dbar t', 
            'MDME(194,1) = 0            !W decay into sbar u', 
            'MDME(195,1) = 0            !W decay into sbar c', 
            'MDME(196,1) = 0            !W decay into sbar t', 
            'MDME(198,1) = 0            !W decay into bbar u', 
            'MDME(199,1) = 0            !W decay into bbar c', 
            'MDME(200,1) = 0            !W decay into bbar t', 
            'MDME(206,1) = 0            !W decay into e+ nu_e', 
            'MDME(207,1) = 0            !W decay into mu+ nu_mu', 
            'MDME(208,1) = 0            !W decay into tau+ nu_tau', 
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
            'MDME(220,1)=1           !Higgs decay into tau nu tau', 
            'MDME(221,1)=0           !Higgs decay into Higgs decay', 
            'MDME(222,1)=0           !Higgs decay into g g', 
            'MDME(223,1)=0           !Higgs decay into gam gam',
            'MDME(224,1)=0           !Higgs decay into gam Z', 
            'MDME(225,1)=0           !Higgs decay into Z Z', 
            'MDME(226,1)=0          !Higgs decay into W W'),
		parameterSets = cms.vstring(
			'pythiaUESettings',
			'processParameters',
		)
	)
)

