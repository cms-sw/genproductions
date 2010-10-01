import FWCore.ParameterSet.Config as cms

source = cms.Source("EmptySource")

from Configuration.Generator.PythiaUEZ2Settings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    comEnergy = cms.double(7000.0),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring(
			'MSEL=0         ! User defined processes', 
            'MSUB(349)=1     ! qq -> H_L++,H_L--', 
            'PMAS(353,1)=90 ! H_L++/-- mass', 
            'MDME(4271,1)=1 ! to 2e', 
            'MDME(4272,1)=1 ! to e,mu', 
            'MDME(4273,1)=1 ! to e,tau', 
            'MDME(4274,1)=1 ! to 2mu', 
            'MDME(4275,1)=1 ! to mu,tau', 
            'MDME(4276,1)=1 ! to 2tau', 
            'MDME(4277,1)=0 ! to 2W', 
            'PARP(181)=0.01 ! ee', 
            'PARP(182)=0.007 ! emu', 
            'PARP(183)=0.007 ! etau', 
            'PARP(184)=0.007 ! mue', 
            'PARP(185)=0.01  ! mumu', 
            'PARP(186)=0.007 ! mutau', 
            'PARP(187)=0.007 ! tau,e', 
            'PARP(188)=0.007 ! taumu', 
            'PARP(189)=0.01  ! tautau', 
            'PARP(192)=0.000000001 ! vev'
		),
        # This is a vector of ParameterSet names to be read, in this order
        parameterSets = cms.vstring('pythiaUESettings', 
            'processParameters')
    )
)

ProductionFilterSequence = cms.Sequence(generator)
