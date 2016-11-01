import FWCore.ParameterSet.Config as cms

from Configuration.Generator.PythiaUEZ2Settings_cfi import *

generator = cms.EDFilter('Pythia6GeneratorFilter',

	filterEfficiency = cms.untracked.double(0.568),
	crossSection = cms.untracked.double(74300.0),

	pythiaHepMCVerbosity = cms.untracked.bool(False),
	pythiaPylistVerbosity = cms.untracked.int32(0),
	maxEventsToPrint = cms.untracked.int32(0),

	comEnergy = cms.double(8000.0),
	ExternalDecays = cms.PSet(
		EvtGen = cms.untracked.PSet(
			operates_on_particles = cms.vint32( 100553, 553 ), 
# 0 (zero) means default list (hardcoded)
# you can put here the list of particles (PDG IDs)
# that you want decayed by EvtGen
			use_default_decay = cms.untracked.bool(False),
			decay_table = cms.FileInPath('GeneratorInterface/ExternalDecays/data/DECAY_NOLONGLIFE.DEC'),
			particle_property_file = cms.FileInPath('GeneratorInterface/ExternalDecays/data/evt.pdl'),
			user_decay_file = cms.FileInPath('GeneratorInterface/ExternalDecays/data/Onia_Upsilon3S_Upsilon1Spipi.dec'),
			list_forced_decays = cms.vstring('MyUpsilon', 'MyUpsilon(2S)'),
			),
		parameterSets = cms.vstring('EvtGen')
	),
	PythiaParameters = cms.PSet(
		pythiaUESettingsBlock,
		processParameters = cms.vstring(
			'MSEL=62          ! Quarkonia', 
			'PMAS(296,1)  = 10.3552    ! change Upsilon(2S) mass to Upsoilon(3S) PDG2006',
			'KFPR(461,1)  = 100553     ! change 461 to Upsilon(3S) + g', 
			'PMAS(365,1)  = 10.3600   ! change bb~ mass larger than Upsilon(3S)',
			'PMAS(366,1)  = 10.3600   ! change bb~ mass larger than Upsilon(3S)',
			'PMAS(367,1)  = 10.3600   ! change bb~ mass larger than Upsilon(3S)',
			'KFDP(4214,1) = 100553     ! bb~ -> Upsilon(2S)',
			'KFDP(4215,1) = 100553     ! bb~ -> Upsilon(2S)',
			'KFDP(4216,1) = 100553     ! bb~ -> Upsilon(2S)',
			'PARP(146)=3.54   ! New values for COM matrix elements',
            		'PARP(147)=0.075  ! New values for COM matrix elements',
            		'PARP(148)=0.001  ! New values for COM matrix elements',
            		'PARP(149)=0.001  ! New values for COM matrix elements',
            		'PARP(150)=0.0    ! New values for COM matrix elements',			 
			'MDME(1578,1) = 0 ! 0.014000    e-              e+', 
			'MDME(1579,1) = 0 ! 0.014000    mu-             mu+', 
			'MDME(1580,1) = 0 ! 0.014000    tau-            tau+', 
			'MDME(1581,1) = 0 ! 0.008000    d               dbar', 
			'MDME(1582,1) = 0 ! 0.024000    u               ubar', 
			'MDME(1583,1) = 0 ! 0.008000    s               sbar', 
			'MDME(1584,1) = 0 ! 0.024000    c               cbar', 
			'MDME(1585,1) = 0 ! 0.425000    g               g            g', 
			'MDME(1586,1) = 0 ! 0.020000    gamma           g            g', 
			'MDME(1587,1) = 0 ! 0.185000    Upsilon         pi+          pi-', 
			'MDME(1588,1) = 0 ! 0.088000    Upsilon         pi0          pi0', 
			'MDME(1589,1) = 0 ! 0.043000    chi_0b          gamma', 
			'MDME(1590,1) = 0 ! 0.067000    chi_1b          gamma', 
			'MDME(1591,1) = 0 ! 0.066000    chi_2b          gamma', 
			'MSTP(142)=2      ! turns on the PYEVWT Pt re-weighting routine', 
			'PARJ(13)=0.750   ! probability that a c or b meson has S=1', 
			'PARJ(14)=0.162   ! probability that a meson with S=0 is produced with L=1, J=1', 
			'PARJ(15)=0.018   ! probability that a meson with S=1 is produced with L=1, J=0', 
			'PARJ(16)=0.054   ! probability that a meson with S=1 is produced with L=1, J=1', 
			'MSTP(145)=0      ! choice of polarization', 
			'MSTP(146)=0      ! choice of polarization frame ONLY when mstp(145)=1', 
			'MSTP(147)=0      ! particular helicity or density matrix component when mstp(145)=1', 
			'MSTP(148)=1      ! possibility to allow for final-state shower evolution, extreme case !', 
			'MSTP(149)=1      ! if mstp(148)=1, it determines the kinematics of the QQ~3S1(8)->QQ~3S1(8)+g branching'),
			parameterSets = cms.vstring('pythiaUESettings', 
				'processParameters', 
				'CSAParameters'),
			CSAParameters = cms.vstring('CSAMODE = 6     ! cross-section reweighted quarkonia')
		)
	)
oniafilter = cms.EDFilter("PythiaFilter",
	Status = cms.untracked.int32(2),
	MaxEta = cms.untracked.double(1e+100),
	MinEta = cms.untracked.double(-1e+100),
	MinPt = cms.untracked.double(0.0),
	ParticleID = cms.untracked.int32(100553)
	)
upsdaufilter = cms.EDFilter("PythiaDauFilter",
        NumberDaughters = cms.untracked.int32(3),
        MaxEta = cms.untracked.double(1e+100),
        MinEta = cms.untracked.double(-1e+100),
        MinPt = cms.untracked.double(0.3),
        ParticleID = cms.untracked.int32(100553),
        DaughterIDs = cms.untracked.vint32(553,211,-211)
        )
mumugenfilter = cms.EDFilter("MCParticlePairFilter",
	Status = cms.untracked.vint32(1, 1),
	MinPt = cms.untracked.vdouble(0.0, 0.0),
	MaxEta = cms.untracked.vdouble(2.5, 2.5),
	MinEta = cms.untracked.vdouble(-2.5, -2.5),
	ParticleCharge = cms.untracked.int32(-1),
	ParticleID1 = cms.untracked.vint32(13),
	ParticleID2 = cms.untracked.vint32(13)
	)

ProductionFilterSequence = cms.Sequence(generator*oniafilter*mumugenfilter*upsdaufilter)

