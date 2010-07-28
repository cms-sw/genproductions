import FWCore.ParameterSet.Config as cms

configurationMetadata = cms.untracked.PSet(
	version = cms.untracked.string('$Revision:$'),
	name = cms.untracked.string('$Source:$'),
	annotation = cms.untracked.string('Herwig+Jimmy generation of QCD events, 7TeV, CTEQ6L1, pthat [10,15] GeV')
)

source = cms.Source("EmptySource")
generator = cms.EDFilter("Herwig6GeneratorFilter",
	comEnergy = cms.double(7000.0),
	crossSection = cms.untracked.double(4.2437E+07),
	doMPInteraction = cms.bool(True),
	emulatePythiaStatusCodes = cms.untracked.bool(True),
	filterEfficiency = cms.untracked.double(1.0),
	herwigHepMCVerbosity = cms.untracked.bool(False),
	herwigVerbosity = cms.untracked.int32(0),
	lhapdfSetPath = cms.untracked.string(''),
	maxEventsToPrint = cms.untracked.int32(0),
	printCards = cms.untracked.bool(False),
	useJimmy = cms.bool(True),

	HerwigParameters = cms.PSet(
		herwigQCDjets = cms.vstring(
			'IPROC     = 1500    ! QCD 2->2 processes', 
			'PTMIN     = 30.    ! minimum pt in hadronic jet',
			'PTMAX     = 50.    ! minimum pt in hadronic jet',
			'MODPDF(1) = 10042   ! PDF set according to LHAGLUE', 
			'MODPDF(2) = 10042   ! CTEQ6L1', 
			'JMUEO     = 1       ! multiparton interaction model',
            'PTJIM     = 4.040   ! 2.8x(sqrt(s)/1.8TeV)^0.27 @ 7 TeV', 
			'JMRAD(73) = 1.8     ! inverse proton radius squared',
			'PRSOF     = 0.0     ! prob. of a soft underlying event',
			'MAXER     = 1000000 ! max error'
		),
		parameterSets = cms.vstring('herwigQCDjets')
	),
)

ProductionFilterSequence = cms.Sequence(generator)


