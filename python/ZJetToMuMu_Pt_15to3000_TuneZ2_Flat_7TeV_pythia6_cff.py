import FWCore.ParameterSet.Config as cms

from Configuration.Generator.PythiaUEZ2Settings_cfi import *


generator = cms.EDFilter('Pythia6GeneratorFilter',
	comEnergy = cms.double(7000.0),
	crossSection = cms.untracked.double(4.941984e+05),
	filterEfficiency = cms.untracked.double(1),
	maxEventsToPrint = cms.untracked.int32(0),
	pythiaHepMCVerbosity = cms.untracked.bool(False),
	pythiaPylistVerbosity = cms.untracked.int32(0),

	PythiaParameters = cms.PSet(
		pythiaUESettingsBlock,
		processParameters = cms.vstring(
			'MSEL = 0        ! user defined processes',
			'MSUB(15) = 1    ! ff -> Z0 f',
			'MSUB(30) = 1    ! ff -> Z0 g',
			'MDME(174,1) = 0 ! Z decay into d dbar',
			'MDME(175,1) = 0 ! Z decay into u ubar',
			'MDME(176,1) = 0 ! Z decay into s sbar',
			'MDME(177,1) = 0 ! Z decay into c cbar',
			'MDME(178,1) = 0 ! Z decay into b bbar',
			'MDME(179,1) = 0 ! Z decay into t tbar',
			'MDME(182,1) = 0 ! Z decay into e- e+',
			'MDME(183,1) = 0 ! Z decay into nu_e nu_ebar',
			'MDME(184,1) = 1 ! Z decay into mu- mu+',
			'MDME(185,1) = 0 ! Z decay into nu_mu nu_mubar',
			'MDME(186,1) = 0 ! Z decay into tau- tau+',
			'MDME(187,1) = 0 ! Z decay into nu_tau nu_taubar' ,
			'CKIN(3) = 15    ! minimum pt hat for hard interactions',
			'CKIN(4) = 3000  ! maximum pt hat for hard interactions',
			'MSTP(142) = 2   ! Turns on the PYWEVT Pt reweighting routine',
		),
		CSAParameters = cms.vstring(
			'CSAMODE = 7     ! towards a flat QCD spectrum',
			'PTPOWER = 4.5   ! reweighting of the pt spectrum',
		),
		parameterSets = cms.vstring(
			'pythiaUESettings',
			'processParameters',
			'CSAParameters',
		)
	)
)

configurationMetadata = cms.untracked.PSet(
	version = cms.untracked.string('\$Revision$'),
	name = cms.untracked.string('\$Source$'),
	annotation = cms.untracked.string('Summer2011 sample with PYTHIA6: Z + Jet production, Z -> mumu, pThat = 15 .. 3000 GeV, TuneZ2')
)
