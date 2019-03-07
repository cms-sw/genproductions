import FWCore.ParameterSet.Config as cms
from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.MCTunes2017.PythiaCP5Settings_cfi import *
from GeneratorInterface.EvtGenInterface.EvtGenSetting_cff import *
# from Configuration.Generator.PSweightsPythia.PythiaPSweightsSettings_cfi import *

# https://cms-pdmv.cern.ch/mcm/public/restapi/requests/get_fragment/BPH-RunIIFall18GS-00123/0

generator = cms.EDFilter("Pythia8GeneratorFilter",
	pythiaPylistVerbosity = cms.untracked.int32(0),
	pythiaHepMCVerbosity = cms.untracked.bool(False),
	comEnergy = cms.double(5020.0),
	maxEventsToPrint = cms.untracked.int32(0),
	ExternalDecays = cms.PSet(
		EvtGen130 = cms.untracked.PSet(
			decay_table = cms.string('GeneratorInterface/EvtGenInterface/data/DECAY_2010.DEC'),
			particle_property_file = cms.FileInPath('GeneratorInterface/EvtGenInterface/data/evt.pdl'),
			user_decay_file = cms.vstring('GeneratorInterface/ExternalDecays/data/Bu_JpsiK.dec'),
			list_forced_decays = cms.vstring('MyB+',
											 'MyB-'),
			operates_on_particles = cms.vint32()
		),
		parameterSets = cms.vstring('EvtGen130')
	),
	PythiaParameters = cms.PSet(
		pythia8CommonSettingsBlock,
        pythia8CP5SettingsBlock,
        # pythia8PSweightsSettingsBlock,
		processParameters = cms.vstring(
			'HardQCD:all = on',
			'PhaseSpace:pTHatMin = 10.',
		),
		parameterSets = cms.vstring(
			'pythia8CommonSettings',
			'pythia8CP5Settings',
			# 'pythia8PSweightsSettings',
			'processParameters',
		)
	)
)

generator.PythiaParameters.processParameters.extend(EvtGenExtraParticles)

###########
# Filters #
###########
mumugenfilter = cms.EDFilter("MCParticlePairFilter",
							 Status = cms.untracked.vint32(1, 1),
							 MinPt = cms.untracked.vdouble(0.0, 0.0),# 0.5, 0.5
							 MinP = cms.untracked.vdouble(0., 0.),
							 MaxEta = cms.untracked.vdouble(10000.0, 10000.0),# 2.5, 2.5
							 MinEta = cms.untracked.vdouble(-10000.0, -10000.0),# -2.5, -2.5
							 MinInvMass = cms.untracked.double(0.0),# 2.0
							 MaxInvMass = cms.untracked.double(100.0),# 4.0
							 ParticleCharge = cms.untracked.int32(-1),
							 ParticleID1 = cms.untracked.vint32(13),
							 ParticleID2 = cms.untracked.vint32(13)
)

BJpsiDaufilter = cms.EDFilter("PythiaMomDauFilter",
							  ParticleID = cms.untracked.int32(521),
							  MomMinPt = cms.untracked.double(0.0),#5.0
							  MomMinEta = cms.untracked.double(-2.4),
							  MomMaxEta = cms.untracked.double(2.4),
							  DaughterIDs = cms.untracked.vint32(443, 321),
							  NumberDaughters = cms.untracked.int32(2),
							  DaughterID = cms.untracked.int32(443),
							  DescendantsIDs = cms.untracked.vint32(13, -13),
							  NumberDescendants = cms.untracked.int32(2),
							  MinEta = cms.untracked.double(-10000.0),#-2.5
							  MaxEta = cms.untracked.double(10000.0),#2.5
)

ProductionFilterSequence = cms.Sequence(generator*mumugenfilter*BJpsiDaufilter)
