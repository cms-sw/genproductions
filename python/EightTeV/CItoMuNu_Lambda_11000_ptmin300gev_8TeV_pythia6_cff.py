import FWCore.ParameterSet.Config as cms

from Configuration.Generator.PythiaUEZ2starSettings_cfi import *

#from Configuration.Generator.PythiaUEZ2Settings_cfi import *

generator = cms.EDFilter('Pythia6GeneratorFilter',
	comEnergy = cms.double(8000.0),
	crossSection = cms.untracked.double(0.02205),
	filterEfficiency = cms.untracked.double(1),
	maxEventsToPrint = cms.untracked.int32(0),
	pythiaHepMCVerbosity = cms.untracked.bool(False),
	pythiaPylistVerbosity = cms.untracked.int32(0),

	PythiaParameters = cms.PSet(
		pythiaUESettingsBlock,
		processParameters = cms.vstring(
			'MSEL=0        ! (D=1) to select between full user control (0, then use MSUB) and some preprogrammed alternative',
			'ITCM(5)=4     ! Switch on contact inteactions for all quarks',
			'RTCM(41)=11000 ! Set Contact Scale Lambda to X TeV',
			'RTCM(42)=1    ! Sign of contact interaction is + (irrelevant for ITCM(5)=3,4)   ',
			'MSUB(166)=1    ! fi+fjbar->fk+flbar (via W+-)',
            'KFPR(166,1)=13    ! mu+nu final state',
			'CKIN(3) = 300  ! minimum pt hat for hard interactions',
		),
		parameterSets = cms.vstring(
			'pythiaUESettings',
			'processParameters',
		)
	)
)

