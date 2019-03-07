import FWCore.ParameterSet.Config as cms
from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.MCTunes2017.PythiaCP5Settings_cfi import *
from GeneratorInterface.EvtGenInterface.EvtGenSetting_cff import *
# from Configuration.Generator.PSweightsPythia.PythiaPSweightsSettings_cfi import *

# https://cms-pdmv.cern.ch/mcm/public/restapi/requests/get_fragment/HIN-HiFall15-00043/0

generator = cms.EDFilter("Pythia8GeneratorFilter",
						 pythiaPylistVerbosity = cms.untracked.int32(0),
						 pythiaHepMCVerbosity = cms.untracked.bool(False),
						 comEnergy = cms.double(5020.0),
						 maxEventsToPrint = cms.untracked.int32(0),
						 ExternalDecays = cms.PSet(
							 EvtGen130 = cms.untracked.PSet(
								 decay_table = cms.string('GeneratorInterface/EvtGenInterface/data/DECAY_2010.DEC'),
								 particle_property_file = cms.FileInPath('GeneratorInterface/EvtGenInterface/data/evt.pdl'),
								 user_decay_file = cms.vstring('GeneratorInterface/ExternalDecays/data/incl_BtoJpsi_mumu.dec'),
								 list_forced_decays = cms.vstring('MyB0', 
																  'Myanti-B0',
																  'MyB+',
																  'MyB-',
																  'MyB_s0', 
																  'Myanti-B_s0'), # why no MyLambda_b0?
								 operates_on_particles = cms.vint32()
							 ),
							 parameterSets = cms.vstring('EvtGen130')
						 ),
                         PythiaParameters = cms.PSet(
							 pythia8CommonSettingsBlock,
							 pythia8CP5SettingsBlock,
							 # pythia8PSweightsSettingsBlock,
							 processParameters = cms.vstring(
								 # 'HardQCD:all = on',
								 'HardQCD:gg2bbbar    = on ',
								 'HardQCD:qqbar2bbbar = on ',
								 'HardQCD:hardbbbar   = on',
								 'PhaseSpace:pTHatMin = 15.',
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
							 MinPt = cms.untracked.vdouble(0.5, 0.5),
							 MinP = cms.untracked.vdouble(0., 0.),
							 MaxEta = cms.untracked.vdouble(2.5, 2.5),
							 MinEta = cms.untracked.vdouble(-2.5, -2.5),
							 MinInvMass = cms.untracked.double(2.0),
							 MaxInvMass = cms.untracked.double(4.0),
							 ParticleCharge = cms.untracked.int32(-1),
							 ParticleID1 = cms.untracked.vint32(13),
							 ParticleID2 = cms.untracked.vint32(13)
)

oniafilter = cms.EDFilter("PythiaFilter",
                          Status = cms.untracked.int32(2),
                          MaxEta = cms.untracked.double(1000.0),
                          MinEta = cms.untracked.double(-1000.0),
                          MinPt = cms.untracked.double(0.0),
                          ParticleID = cms.untracked.int32(443)
)

bfilter = cms.EDFilter("PythiaFilter",
					   ParticleID = cms.untracked.int32(5)
)

ProductionFilterSequence = cms.Sequence(generator*mumugenfilter*oniafilter*bfilter)