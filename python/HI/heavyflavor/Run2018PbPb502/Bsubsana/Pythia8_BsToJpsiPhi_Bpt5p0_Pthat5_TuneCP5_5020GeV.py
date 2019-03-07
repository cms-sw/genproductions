import FWCore.ParameterSet.Config as cms
from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.MCTunes2017.PythiaCP5Settings_cfi import *
from GeneratorInterface.EvtGenInterface.EvtGenSetting_cff import *
# from Configuration.Generator.PSweightsPythia.PythiaPSweightsSettings_cfi import *

# https://cms-pdmv.cern.ch/mcm/public/restapi/requests/get_fragment/BPH-RunIIFall17GS-00115/0

generator = cms.EDFilter("Pythia8GeneratorFilter",
	comEnergy = cms.double(5020.0),
	crossSection = cms.untracked.double(54000000000),
	filterEfficiency = cms.untracked.double(3.0e-4),
	pythiaHepMCVerbosity = cms.untracked.bool(False),
	maxEventsToPrint = cms.untracked.int32(0),
	pythiaPylistVerbosity = cms.untracked.int32(0),
	ExternalDecays = cms.PSet(
		EvtGen130 = cms.untracked.PSet(
			decay_table = cms.string('GeneratorInterface/EvtGenInterface/data/DECAY_2010.DEC'), # diff w/o NOLONGLIFE
			# particle_property_file = cms.FileInPath('GeneratorInterface/EvtGenInterface/data/evt.pdl'),
			particle_property_file = cms.FileInPath('GeneratorInterface/EvtGenInterface/data/evt.pdl'), # after (including) CMSSW_9_4_12
			# user_decay_file = cms.vstring('GeneratorInterface/ExternalDecays/data/Bs_JpsiPhi_V3.dec'),
			list_forced_decays = cms.vstring('MyB_s0','Myanti-B_s0'),
			user_decay_embedded = cms.vstring(
"""
# https://hypernews.cern.ch/HyperNews/CMS/get/bphysics/1956/2.html
Define betas   0.015
Define Apara   0.475
Define Azero   0.724
Define Aperp   0.500
Define pApara  3.26
Define pAzero  0.0
Define pAperp  3.08
#
Alias MyB_s0 B_s0
Alias Myanti-B_s0 anti-B_s0
ChargeConj Myanti-B_s0 MyB_s0
Alias      MyJ/psi  J/psi
Alias      MyPhi    phi
ChargeConj MyJ/psi  MyJ/psi
ChargeConj MyPhi    MyPhi
#
Decay MyB_s0
  1.000         MyJ/psi     MyPhi        PVV_CPLH betas 1 Apara pApara Azero pAzero Aperp pAperp;
#
Enddecay
Decay Myanti-B_s0
  1.000         MyJ/psi     MyPhi        PVV_CPLH betas 1 Apara pApara Azero pAzero Aperp pAperp;
Enddecay
#
Decay MyJ/psi
  1.000         mu+         mu-          PHOTOS VLL;
Enddecay
#
Decay MyPhi
  1.000         K+          K-           VSS;
Enddecay
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
			'HardQCD:all = on',
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
							  ParticleID = cms.untracked.int32(531),
							  MomMinPt = cms.untracked.double(5.),
							  MomMinEta = cms.untracked.double(-2.5),
							  MomMaxEta = cms.untracked.double(2.5),
							  DaughterIDs = cms.untracked.vint32(443, 333),
							  NumberDaughters = cms.untracked.int32(2),
							  DaughterID = cms.untracked.int32(443),
							  DescendantsIDs = cms.untracked.vint32(13, -13),
							  NumberDescendants = cms.untracked.int32(2),
							  MinEta = cms.untracked.double(-2.5),
							  MaxEta = cms.untracked.double(2.5),
)

BPhiDaufilter = cms.EDFilter("PythiaMomDauFilter",
							 ParticleID = cms.untracked.int32(531),
							 MomMinPt = cms.untracked.double(5.),
							 MomMinEta = cms.untracked.double(-2.5),
							 MomMaxEta = cms.untracked.double(2.5),
							 DaughterIDs = cms.untracked.vint32(443, 333),
							 NumberDaughters = cms.untracked.int32(2),
							 DaughterID = cms.untracked.int32(333),
							 DescendantsIDs = cms.untracked.vint32(321, -321),
							 NumberDescendants = cms.untracked.int32(2),
							 MinEta = cms.untracked.double(-2.5),
							 MaxEta = cms.untracked.double(2.5),
)

ProductionFilterSequence = cms.Sequence(generator*mumugenfilter*BJpsiDaufilter*BPhiDaufilter)
