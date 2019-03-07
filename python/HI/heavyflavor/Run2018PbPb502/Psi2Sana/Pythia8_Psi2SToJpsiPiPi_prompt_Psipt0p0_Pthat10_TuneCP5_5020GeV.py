import FWCore.ParameterSet.Config as cms
from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.MCTunes2017.PythiaCP5Settings_cfi import *
from GeneratorInterface.EvtGenInterface.EvtGenSetting_cff import *
# from Configuration.Generator.PSweightsPythia.PythiaPSweightsSettings_cfi import *

# https://cms-pdmv.cern.ch/mcm/public/restapi/requests/get_fragment/HIN-HiFall15-00038/0

generator = cms.EDFilter("Pythia8GeneratorFilter",
						 pythiaPylistVerbosity = cms.untracked.int32(0),
						 pythiaHepMCVerbosity = cms.untracked.bool(False),
						 comEnergy = cms.double(5020.0),
						 maxEventsToPrint = cms.untracked.int32(0), # 
						 ExternalDecays = cms.PSet(
							 EvtGen130 = cms.untracked.PSet(
								 decay_table = cms.string('GeneratorInterface/EvtGenInterface/data/DECAY_2010.DEC'),
								 particle_property_file = cms.FileInPath('GeneratorInterface/EvtGenInterface/data/evt.pdl'),
								 user_decay_file = cms.vstring('GeneratorInterface/ExternalDecays/data/incl_BtoPsi2S_Jpsipipi.dec'),
								 list_forced_decays = cms.vstring('Mypsi(2S)'),
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
								 'Charmonium:states(3S1) = 100443', # filter on 100443 and prevents other onium states decaying to 443, so we should turn the others off
								 'Charmonium:O(3S1)[3S1(1)] = 0.76', # The colour-singlet long-distance matrix elements <O[3S1(1)]> for the 3S1 charmonium states
								 'Charmonium:O(3S1)[3S1(8)] = 0.005', # The colour-octet long-distance matrix elements <O[3S1(8)]> for the 3S1 charmonium states
								 'Charmonium:O(3S1)[1S0(8)] = 0.004', # The colour-octet long-distance matrix elements <O[1S0(8)]> for the 3S1 charmonium states
								 'Charmonium:O(3S1)[3P0(8)] = 0.004', # The colour-octet long-distance matrix elements <O[3P0(8)]>/m_Q^2 for the 3S1 charmonium states
								 'Charmonium:gg2ccbar(3S1)[3S1(1)]g = on', # Colour-singlet production of 3S1 charmonium states via g g -> ccbar[3S1(1)] g. Code 401.
								 'Charmonium:gg2ccbar(3S1)[3S1(8)]g = on', # Colour-octet production of 3S1 charmonium states via g g -> ccbar[3S1(8)] g. Code 402.
								 'Charmonium:qg2ccbar(3S1)[3S1(8)]q = on', # Colour-octet production of 3S1 charmonium states via q g -> ccbar[3S1(8)] q. Code 403.
								 'Charmonium:qqbar2ccbar(3S1)[3S1(8)]g = on', # Colour-octet production of 3S1 charmonium states via q qbar -> ccbar[3S1(8)] g. Code 404.
								 'Charmonium:gg2ccbar(3S1)[1S0(8)]g = on', # Colour-octet production of 3S1 charmonium states via g g -> ccbar[1S0(8)] g. Code 405.
								 'Charmonium:qg2ccbar(3S1)[1S0(8)]q = on', # Colour-octet production of 3S1 charmonium states via q g -> ccbar[1S0(8)] q. Code 406.
								 'Charmonium:qqbar2ccbar(3S1)[1S0(8)]g = on', # Colour-octet production of 3S1 charmonium states via q qbar -> ccbar[1S0(8)] g. Code 407.
								 'Charmonium:gg2ccbar(3S1)[3PJ(8)]g = on', # Colour-octet production of 3S1 charmonium states via g g -> ccbar[3PJ(8)] g. Code 408.
								 'Charmonium:qg2ccbar(3S1)[3PJ(8)]q = on', # Colour-octet production of 3S1 charmonium states via q g -> ccbar[3PJ(8)] q. Code 409.
								 'Charmonium:qqbar2ccbar(3S1)[3PJ(8)]g = on', # Colour-octet production of 3S1 charmonium states via q qbar -> ccbar[3SJ(8)] g. Code 410.
								 'Charmonium:gg2ccbar(3S1)[3S1(1)]gm = on', # [new] Colour-singlet production of 3S1 charmonium states via g g -> ccbar[3S1(1)] g with a hard gamma. Code 441.
								 '100443:onMode = off',      # ignore cross-section re-weighting (CSAMODE=6) since selecting wanted decay mode 
								 # '100443:onIfAny = 13 -13', # What's this?
								 'PhaseSpace:pTHatMin = 10.'
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

Psi2SJpsiDaufilter = cms.EDFilter("PythiaMomDauFilter",
								  ParticleID = cms.untracked.int32(100443),
								  MomMinPt = cms.untracked.double(0.),
								  MomMinEta = cms.untracked.double(-2.4),
								  MomMaxEta = cms.untracked.double(2.4),
								  DaughterIDs = cms.untracked.vint32(443, 211, -211),
								  NumberDaughters = cms.untracked.int32(3),
								  NumberDescendants = cms.untracked.int32(0),
)

# cfilter = cms.EDFilter("PythiaFilter",
# 					   ParticleID = cms.untracked.int32(4)
# )

ProductionFilterSequence = cms.Sequence(generator*mumugenfilter*Psi2SJpsiDaufilter)
