import FWCore.ParameterSet.Config as cms

from Configuration.Generator.PythiaUESettings_cfi import *

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.2 $'),
    annotation = cms.untracked.string('Bc to J/psi a1 from BCVEGPY LHE file and EvtGen'),
    name = cms.untracked.string('$Source: genbcjpsia1_EvtGen.py')
)


generator = cms.EDFilter("Pythia6HadronizerFilter",
    maxEventsToPrint = cms.untracked.int32(2),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    comEnergy = cms.double(7000.),
    ExternalDecays = cms.PSet(
        EvtGen = cms.untracked.PSet(
             operates_on_particles = cms.vint32( 0 ),
             use_default_decay = cms.untracked.bool(False),
             decay_table = cms.FileInPath('GeneratorInterface/ExternalDecays/data/DECAY_NOLONGLIFE.DEC'),
              particle_property_file = cms.FileInPath('GeneratorInterface/ExternalDecays/data/evt.pdl'),
              user_decay_file = cms.FileInPath('GeneratorInterface/ExternalDecays/data/Bc_Jpsia1.dec'),
              list_forced_decays = cms.vstring('MyBc+','MyBc-'),
              ),
         parameterSets = cms.vstring('EvtGen')
    ),    
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring(
            'MSTP(61)=0             ! Hadronization of the initial protons initial-state QCD and QED radiation OFF',
            'MDME(1027,2) = 0        ! PHASE SPACE', 
            'KFDP(1027,1) = 443      ! J/psi', 
            'KFDP(1027,2) = 20213    ! a_1+', 
            'KFDP(1027,3) = 0        ! nada', 
            'KFDP(1027,4) = 0        ! nada', 
            'KFDP(1027,5) = 0        ! nada', 
            'PMAS(143,1) = 6.275', 
            'PMAS(143,4) = 0.138',
	        'MDME(858,1) = 0         ! J/psi->e+e-', 
            'MDME(859,1) = 1         ! J/psi->mumu', 
            'MDME(860,1) = 0         ! J/psi->quarks',
            'MDME(997,1) = 3         ! on for Bc- only, antiparticle',
	        'MDME(998,1) = 3', 
            'MDME(999,1) = 3', 
            'MDME(1000,1) = 3', 
            'MDME(1001,1) = 3', 
            'MDME(1002,1) = 3', 
            'MDME(1003,1) = 3', 
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
            'MDME(1027,1) = 2        ! on for Bc+ -> J/psi pi+ only, particle'), 	
        parameterSets = cms.vstring('pythiaUESettings','processParameters')
    )
)

ProductionFilterSequence = cms.Sequence(generator)
