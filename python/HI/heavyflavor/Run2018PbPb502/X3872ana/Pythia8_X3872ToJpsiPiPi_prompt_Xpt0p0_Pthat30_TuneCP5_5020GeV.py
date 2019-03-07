import FWCore.ParameterSet.Config as cms
from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.MCTunes2017.PythiaCP5Settings_cfi import *
from GeneratorInterface.EvtGenInterface.EvtGenSetting_cff import *
# from Configuration.Generator.PSweightsPythia.PythiaPSweightsSettings_cfi import *

# https://cms-pdmv.cern.ch/mcm/public/restapi/requests/get_fragment/BPH-RunIIFall18GS-00094/0

generator = cms.EDFilter("Pythia8GeneratorFilter",
						 pythiaPylistVerbosity = cms.untracked.int32(0),
						 pythiaHepMCVerbosity = cms.untracked.bool(False),
						 comEnergy = cms.double(5020.0),
						 maxEventsToPrint = cms.untracked.int32(0),
# 						 ExternalDecays = cms.PSet(
# 							 EvtGen130 = cms.untracked.PSet(
# 								 decay_table = cms.string('GeneratorInterface/EvtGenInterface/data/DECAY_2014_NOLONGLIFE.DEC'),
# 								 particle_property_file = cms.FileInPath('GeneratorInterface/EvtGenInterface/data/evt_2014.pdl'),
# 								 # user_decay_file = cms.vstring('GeneratorInterface/ExternalDecays/data/Onia_mumu_withX3872.dec'),
# 								 list_forced_decays = cms.vstring('myX3872'),
# 								 user_decay_embedded = cms.vstring(
# """
# #
# Alias      MyJ/psi  J/psi
# ChargeConj MyJ/psi  MyJ/psi
# #
# Decay MyJ/psi
#   1.000         mu+       mu-            PHOTOS VLL;
# Enddecay
# #
# #
# Alias      myX3872  chi_c1
# ChargeConj myX3872  myX3872
# Particle myX3872 3.872 0.003
# #
# Decay myX3872
#   1.000         MyJ/psi      pi+     pi-          PHSP;
# Enddecay
# End
# """
# 								 ),
# 								 operates_on_particles = cms.vint32()
# 							 ),
# 							 parameterSets = cms.vstring('EvtGen130')
# 						 ),
                         PythiaParameters = cms.PSet(
							 pythia8CommonSettingsBlock,
							 pythia8CP5SettingsBlock,
                             # pythia8PSweightsSettingsBlock,
							 processParameters = cms.vstring(
								 # 'HardQCD:all = on',
								 'Charmonium:states(3PJ) = 20443', # generating only Chi_c1 particle
								 'Charmonium:O(3PJ)[3P0(1)] = 0.05', # The color-singlet long-distance matrix elements <O[3P0(1)]>/m_Q^2 for the 3PJ charmonium states
								 'Charmonium:O(3PJ)[3S1(8)] = 0.0031', # The color-singlet long-distance matrix elements <O[3S1(8)]> for the 3PJ charmonium states.
								 'Charmonium:gg2ccbar(3PJ)[3PJ(1)]g = on', # Colour-singlet production of 3PJ charmonium states via g g → ccbar[3PJ(1)] g. Code 411.
								 'Charmonium:qg2ccbar(3PJ)[3PJ(1)]q = on', # Colour-singlet production of 3PJ charmonium states via q g → ccbar[3PJ(1)] q. Code 412.
								 'Charmonium:qqbar2ccbar(3PJ)[3PJ(1)]g = on', # Colour-singlet production of 3PJ charmonium states via q qbar → ccbar[3PJ(1)] g. Code 413.
								 'Charmonium:gg2ccbar(3PJ)[3S1(8)]g = on', # Colour-octet production of 3PJ charmonium states via g g → ccbar[3S1(8)] g. Code 414.
								 'Charmonium:qg2ccbar(3PJ)[3S1(8)]q = on', # Colour-octet production of 3PJ charmonium states via q g → ccbar[3S1(8)] q. Code 415.
								 'Charmonium:qqbar2ccbar(3PJ)[3S1(8)]g = on', # Colour-octet production of 3PJ charmonium states via q qbar → ccbar[3S1(8)] g. Code 416.
								 '20443:m0=3.87169',
                                 '20443:mWidth=0.0012',
                                 '20443:mMin=3.868',
                                 '20443:mMax=3.874',
                                 '20443:addChannel = on 1 0 443 211 -211',
                                 '20443:onMode = off',
                                 '20443:onIfMatch = 443 211 -211',
                                 '443:onMode = off',
                                 '443:onIfMatch = 13 -13',
								 'PhaseSpace:pTHatMin = 30.',
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

BJpsiDaufilter = cms.EDFilter("PythiaMomDauFilter",
    ParticleID = cms.untracked.int32(20443),
    MomMinPt = cms.untracked.double(15.),
    MomMinEta = cms.untracked.double(-2.4),
    MomMaxEta = cms.untracked.double(2.4),
    DaughterIDs = cms.untracked.vint32(443, 211, -211),
    NumberDaughters = cms.untracked.int32(3),
    DaughterID = cms.untracked.int32(443),
    DescendantsIDs = cms.untracked.vint32(13, -13),
    NumberDescendants = cms.untracked.int32(2),
    MinEta = cms.untracked.double(-2.5),
    MaxEta = cms.untracked.double(2.5),
)

# BX3872Daufilter = cms.EDFilter("PythiaMomDauFilter",
#     ParticleID = cms.untracked.int32(20443),
#     MomMinPt = cms.untracked.double(15.),
#     MomMinEta = cms.untracked.double(-2.4),
#     MomMaxEta = cms.untracked.double(2.4),
#     DaughterIDs = cms.untracked.vint32(443, 113),
#     NumberDaughters = cms.untracked.int32(2),
#     DaughterID = cms.untracked.int32(113),
#     DescendantsIDs = cms.untracked.vint32(211, -211),
#     NumberDescendants = cms.untracked.int32(2),
# )

ProductionFilterSequence = cms.Sequence(generator*mumugenfilter*BJpsiDaufilter)