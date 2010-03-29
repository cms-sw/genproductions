import FWCore.ParameterSet.Config as cms
# import of standard configurations
from Configuration.GenProduction.PythiaUESettings_cfi import *

source = cms.Source("LHESource",
    fileNames = cms.untracked.vstring("file:BCVEGPY.lhe"),
    # skipEvents = cms.untracked.uint32(3), ## skip the first 3 events,
)
generator = cms.EDFilter("Pythia6HadronizerFilter",
		pythiaPylistVerbosity = cms.untracked.int32(1),
		pythiaHepMCVerbosity = cms.untracked.bool(True),
      crossSection = cms.untracked.double(178.6),
      filterEfficiency = cms.untracked.double(0.46),
		comEnergy = cms.double(7000.0),
		eventsToPrint = cms.untracked.int32(1),

		PythiaParameters = cms.PSet(
			pythiaUESettingsBlock,   
			processParameters = cms.vstring(
				'MSTP(61)=0             ! Hadronization of the initial protons', 
				'MDME(997,2) = 0        ! PHASE SPACE', 
				'BRAT(997)   = 1.       ! BRANCHING FRACTION', 
				'KFDP(997,1) = 211       ! pi+', 
				'KFDP(997,2) = 443      ! J/psi', 
				'KFDP(997,3) = 0       ! nada', 
				'KFDP(997,4) = 0        ! nada', 
				'KFDP(997,5) = 0        ! nada', 
				'PMAS(143,1) = 6.276', 
				'PMAS(143,4) = 0.138', 
				'MDME(858,1) = 0  ! J/psi->e+e-', 
				'MDME(859,1) = 1  ! J/psi->mumu', 
				'MDME(860,1) = 0', 
            'MDME(997,1) = 2   !  Bc -> pi J/Psi'
				'MDME(998,1) = 3', 
				'MDME(999,1) = 3', 
				'MDME(1000,1) = 3', 
				'MDME(1001,1) = 3', 
				'MDME(1002,1) = 3', 
				'MDME(1003,1) = 3   ! Bc -> J/PsiMuNu', 
				'MDME(1004,1) = 3', 
				'MDME(1005,1) = 3', 
				'MDME(1006,1) = 3', 
				'MDME(1007,1) = 3', 
				'MDME(1008,1) = 3', 
				'MDME(1009,1) = 3', 
				'MDME(1010,1) = 3', 
				'MDME(1011,1) = 3', 
				'MDME(1012,1) = 3', 
				'MDME(1013,1) = 3', 
				'MDME(1014,1) = 3', 
				'MDME(1015,1) = 3', 
				'MDME(1016,1) = 3', 
				'MDME(1017,1) = 3', 
				'MDME(1018,1) = 3', 
				'MDME(1019,1) = 3', 
				'MDME(1020,1) = 3', 
				'MDME(1021,1) = 3', 
				'MDME(1022,1) = 3', 
				'MDME(1023,1) = 3', 
				'MDME(1024,1) = 3', 
				'MDME(1025,1) = 3', 
				'MDME(1026,1) = 3', 
				'MDME(1027,1) = 3'),
				parameterSets = cms.vstring('pythiaUESettings', 
						'processParameters')					
	)
	
)
oniafilter = cms.EDFilter("PythiaFilter",
    MaxEta = cms.untracked.double(1000.0),
    Status = cms.untracked.int32(2),
    MinEta = cms.untracked.double(-1000.0),
    MinPt = cms.untracked.double(0.0),
    ParticleID = cms.untracked.int32(443)
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
mugenfilter = cms.EDFilter("MCSmartSingleParticleFilter",
    Status = cms.untracked.vint32(1, 1),
    MaxDecayRadius = cms.untracked.vdouble(1500.0, 1500.0),
    MinPt = cms.untracked.vdouble(2.5, 2.5),
    ParticleID = cms.untracked.vint32(13, -13),
    MaxEta = cms.untracked.vdouble(2.5, 2.5),
    MinEta = cms.untracked.vdouble(-2.5, -2.5),
    MaxDecayZ = cms.untracked.vdouble(3000.0, 3000.0),
    MinDecayZ = cms.untracked.vdouble(-3000.0, -3000.0)
)

ProducerSourceSequence = cms.Sequence(generator*oniafilter*mumugenfilter*mugenfilter)

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    annotation = cms.untracked.string('Bcvegpy BctoJpsipi channel at 7Tev'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/Bcvegpy_BctoJPsipi_LHEProducer_7Tev_cff.py,v $')
)

