import FWCore.ParameterSet.Config as cms

from Configuration.Generator.PythiaUED6TSettings_cfi import *

generator = cms.EDFilter('Pythia6GeneratorFilter',
	comEnergy = cms.double(7000.0),
	crossSection = cms.untracked.double(2.464313e+10),
	filterEfficiency = cms.untracked.double(1),
	maxEventsToPrint = cms.untracked.int32(0),
	pythiaHepMCVerbosity = cms.untracked.bool(False),
	pythiaPylistVerbosity = cms.untracked.int32(0),

	PythiaParameters = cms.PSet(
		pythiaUESettingsBlock,
		processParameters = cms.vstring(
			'MSEL=0        ! (D=1) to select between full user control (0, then use MSUB) and some preprogrammed alternative',
			'ITCM(5)=2     ! Switch on contact inteactions for all quarks',
			'RTCM(41)=3000 ! Set Contact Scale Lambda to 3 TeV',
			'RTCM(42)=1    ! Sign of contact interaction is +',
			'MSUB(381)=1   ! qi qj -> qi qj via QCD plus a contact interaction',
			'MSUB(382)=1   ! qi qiBar -> qk qkBar via QCD plus a contact interaction',
			'MSUB(13)=1    ! qi qiBar -> g g via normal QCD',
			'MSUB(28)=1    ! qi g -> qi g  via normal QCD',
			'MSUB(53)=1    ! g g -> qk qkbar via normal QCD',
			'MSUB(68)=1    ! g g -> g g via normal QCD',
			'CKIN(3) = 15    ! minimum pt hat for hard interactions',
			'CKIN(4) = 3000  ! maximum pt hat for hard interactions',
			'MSTP(142) = 2   ! Turns on the PYWEVT Pt reweighting routine',
		),
		CSAParameters = cms.vstring(
			'CSAMODE = 7     ! towards a flat QCD spectrum',
			'PTPOWER = 4.5   ! reweighting of the pt spectrum',
		),
		parameterSets = cms.vstring(
			'pythiaUESettings',
			'processParameters',
			'CSAParameters',
		)
	)
)

configurationMetadata = cms.untracked.PSet(
	version = cms.untracked.string('\$Revision$'),
	name = cms.untracked.string('\$Source$'),
	annotation = cms.untracked.string('Fall2010 sample with PYTHIA6: QCD + 3TeV contact interaction, pThat = 15 .. 3000 GeV, weighted, TuneD6T')
)
