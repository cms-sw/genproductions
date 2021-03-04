import FWCore.ParameterSet.Config as cms

from Configuration.Generator.PythiaUEZ2starSettings_cfi import *


generator = cms.EDFilter('Pythia6GeneratorFilter',
	comEnergy = cms.double(8000.0),
	crossSection = cms.untracked.double(3.060099e+03),
	filterEfficiency = cms.untracked.double(0.675889),
	maxEventsToPrint = cms.untracked.int32(0),
	pythiaHepMCVerbosity = cms.untracked.bool(False),
	pythiaPylistVerbosity = cms.untracked.int32(0),

	PythiaParameters = cms.PSet(
		pythiaUESettingsBlock,
		processParameters = cms.vstring(
			'MSEL = 0',
			'MSUB(1) = 1',
			'MSTP(43) = 3    ! both Z0 and gamma*',
			'MDME(174,1) = 0 ! Z decay into d dbar',
			'MDME(175,1) = 0 ! Z decay into u ubar',
			'MDME(176,1) = 0 ! Z decay into s sbar',
			'MDME(177,1) = 1 ! Z decay into c cbar',
			'MDME(178,1) = 0 ! Z decay into b bbar',
			'MDME(179,1) = 0 ! Z decay into t tbar',
			'MDME(182,1) = 0 ! Z decay into e- e+',
			'MDME(183,1) = 0 ! Z decay into nu_e nu_ebar',
			'MDME(184,1) = 0 ! Z decay into mu- mu+',
			'MDME(185,1) = 0 ! Z decay into nu_mu nu_mubar',
			'MDME(186,1) = 0 ! Z decay into tau- tau+',
			'MDME(187,1) = 0 ! Z decay into nu_tau nu_taubar' ,
			'CKIN(1) = 50    ! Minimum sqrt(s_hat) value (=Z mass)'
		),
		parameterSets = cms.vstring(
			'pythiaUESettings',
			'processParameters',
		)
	)
)

ccbargenfilter = cms.EDFilter('MCParticlePairFilter',
	Status = cms.untracked.vint32(3, 3),
	MinPt = cms.untracked.vdouble(1.0, 1.0),
	MaxEta = cms.untracked.vdouble(3.0, 3.0),
	MinEta = cms.untracked.vdouble(-3.0, -3.0),
	ParticleCharge = cms.untracked.int32(-1),
	ParticleID1 = cms.untracked.vint32(4),
	ParticleID2 = cms.untracked.vint32(4)
)

ProductionFilterSequence = cms.Sequence(generator * ccbargenfilter)

configurationMetadata = cms.untracked.PSet(
	version = cms.untracked.string('\$Revision$'),
	name = cms.untracked.string('\$Source$'),
	annotation = cms.untracked.string('Summer2012-Z2star sample with PYTHIA6: Drell-Yan Z/g* -> cc, m > 50 GeV, TuneZ2star')
)
