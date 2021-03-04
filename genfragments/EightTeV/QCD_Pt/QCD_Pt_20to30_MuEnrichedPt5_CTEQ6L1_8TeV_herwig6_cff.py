import FWCore.ParameterSet.Config as cms

configurationMetadata = cms.untracked.PSet(
	version = cms.untracked.string('$Revision: 1.1 $'),
	name = cms.untracked.string('$Source: /local/reps/CMSSW/CMSSW/Configuration/GenProduction/python/EightTeV/QCD_Pt_20to30_MuEnrichedPt5_CTEQ6L1_8TeV_herwig6_cff.py,v $'),
	annotation = cms.untracked.string('Herwig+Jimmy generation of QCD events, MuEnrichedPt5, 8 TeV, CTEQ6L1, pthat [20,30] GeV')
)

source = cms.Source("EmptySource")
generator = cms.EDFilter("Herwig6GeneratorFilter",
	comEnergy = cms.double(8000.0),
	crossSection = cms.untracked.double(230470000.),
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
			'PTMIN     = 20.    ! minimum pt in hadronic jet',
			'PTMAX     = 30.    ! minimum pt in hadronic jet',
			'MODPDF(1) = 10042   ! PDF set according to LHAGLUE', 
			'MODPDF(2) = 10042   ! CTEQ6L1', 
			'JMUEO     = 1       ! multiparton interaction model',
                        'PTJIM     = 4.189   ! 2.8x(sqrt(s)/1.8TeV)^0.27 @ 8 TeV', 
			'JMRAD(73) = 1.8     ! inverse proton radius squared',
			'PRSOF     = 0.0     ! prob. of a soft underlying event',
			'MAXER     = 1000000 ! max error'
		),
		parameterSets = cms.vstring('herwigQCDjets')
	),
)

mugenfilter = cms.EDFilter("MCSmartSingleParticleFilter",
    MinPt = cms.untracked.vdouble(5.,5.),
    MinEta = cms.untracked.vdouble(-2.5,-2.5),
    MaxEta = cms.untracked.vdouble(2.5,2.5),
    ParticleID = cms.untracked.vint32(13,-13),
    Status = cms.untracked.vint32(1,1),
    # Decay cuts are in mm
    MaxDecayRadius = cms.untracked.vdouble(2000.,2000.),
    MinDecayZ = cms.untracked.vdouble(-4000.,-4000.),
    MaxDecayZ = cms.untracked.vdouble(4000.,4000.)
)

ProductionFilterSequence = cms.Sequence(generator*mugenfilter)


