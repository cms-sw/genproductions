import FWCore.ParameterSet.Config as cms
from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.MCTunes2017.PythiaCP5Settings_cfi import *
from GeneratorInterface.EvtGenInterface.EvtGenSetting_cff import *
# from Configuration.Generator.PSweightsPythia.PythiaPSweightsSettings_cfi import *

# https://cms-pdmv.cern.ch/mcm/public/restapi/requests/get_fragment/BPH-RunIIFall17GS-00134
# https://cms-pdmv.cern.ch/mcm/public/restapi/requests/get_fragment/BPH-RunIIFall17GS-00132

generator = cms.EDFilter("Pythia8GeneratorFilter",
						 pythiaPylistVerbosity = cms.untracked.int32(0),
						 pythiaHepMCVerbosity = cms.untracked.bool(False),
						 comEnergy = cms.double(5020.0),
						 maxEventsToPrint = cms.untracked.int32(0),
						 ExternalDecays = cms.PSet(
							 EvtGen130 = cms.untracked.PSet(
								 decay_table = cms.string('GeneratorInterface/EvtGenInterface/data/DECAY_2010.DEC'),
								 particle_property_file = cms.FileInPath('GeneratorInterface/EvtGenInterface/data/evt.pdl'),
								 # user_decay_file = cms.vstring('GeneratorInterface/ExternalDecays/data/incl_BtoX3872_Jpsipipi.dec'),
								 list_forced_decays = cms.vstring('MyB+',
																  'MyB-',
																  'MyB0',
																  'Myanti-B0'),
								 user_decay_embedded = cms.vstring(
"""
# https://github.com/cms-data/GeneratorInterface-EvtGenInterface/blob/master/incl_BtoX3872_Jpsipipi.dec
Alias      MyJ/psi  J/psi
ChargeConj MyJ/psi  MyJ/psi
#
Decay MyJ/psi
  1.000         mu+       mu-            PHOTOS VLL;
Enddecay
#
#
Alias Myrho0 rho0
ChargeConj Myrho0 Myrho0
#
Decay Myrho0
1.000    pi+ pi-                         VSS;
Enddecay
#
#
Alias      myX(3872)  chi_c1
# Alias      myX(3872)  X(3872)
ChargeConj myX(3872)  myX(3872)
Particle myX(3872) 3.87169 0.0001

#
Decay myX(3872)
  1.000         MyJ/psi      Myrho0          PHSP;
Enddecay

#
Alias      MyB+   B+
Alias      MyB-   B-
ChargeConj MyB-   MyB+
#
Decay MyB+
  0.333         myX(3872)      K+          SVS;
  0.334         myX(3872)      K0 pi+      PHSP;
  0.333         myX(3872)      K*+         SVV_HELAMP PKHplus PKphHplus PKHzero PKphHzero PKHminus PKphHminus;
Enddecay
Decay MyB-
  0.333         myX(3872)      K-          SVS;
  0.334         myX(3872)      anti-K0 pi- PHSP;
  0.333         myX(3872)      K*-         SVV_HELAMP PKHplus PKphHplus PKHzero PKphHzero PKHminus PKphHminus;
Enddecay

#
Alias      MyB0  B0
Alias      Myanti-B0  anti-B0
ChargeConj MyB0  Myanti-B0
#
Decay MyB0
  0.352         myX(3872)      K0         SVS;
  0.328         myX(3872)      K*0        SVV_HELAMP PKHplus PKphHplus PKHzero PKphHzero PKHminus PKphHminus;
  0.320         myX(3872)      K+ pi-     PHSP;
Enddecay
#
Decay Myanti-B0
  0.352         myX(3872)      anti-K0          SVS;
  0.328         myX(3872)      anti-K*0         SVV_HELAMP PKHplus PKphHplus PKHzero PKphHzero PKHminus PKphHminus;
  0.320         myX(3872)      K- pi+           PHSP;
Enddecay

#
End
"""	 
								 ),
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
								 # "SoftQCD:nonDiffractive = on",
								 'HardQCD:gg2bbbar    = on ',
								 'HardQCD:qqbar2bbbar = on ',
                                 'HardQCD:hardbbbar   = on',
								 'PhaseSpace:pTHatMin = 5.',
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
    MomMinPt = cms.untracked.double(0.),
    MomMinEta = cms.untracked.double(-2.4),
    MomMaxEta = cms.untracked.double(2.4),
    DaughterIDs = cms.untracked.vint32(443, 113),
    NumberDaughters = cms.untracked.int32(2),
    DaughterID = cms.untracked.int32(443),
    DescendantsIDs = cms.untracked.vint32(13, -13),
    NumberDescendants = cms.untracked.int32(2),
    MinEta = cms.untracked.double(-2.5),
    MaxEta = cms.untracked.double(2.5),
)

BX3872Daufilter = cms.EDFilter("PythiaMomDauFilter",
    ParticleID = cms.untracked.int32(20443),
    MomMinPt = cms.untracked.double(0.),
    MomMinEta = cms.untracked.double(-2.4),
    MomMaxEta = cms.untracked.double(2.4),
    DaughterIDs = cms.untracked.vint32(443, 113),
    NumberDaughters = cms.untracked.int32(2),
    DaughterID = cms.untracked.int32(113),
    DescendantsIDs = cms.untracked.vint32(211, -211),
    NumberDescendants = cms.untracked.int32(2),
)

bfilter = cms.EDFilter("PythiaFilter",
                       ParticleID = cms.untracked.int32(5)
)

ProductionFilterSequence = cms.Sequence(generator*mumugenfilter*BJpsiDaufilter*BX3872Daufilter*bfilter)