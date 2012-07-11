import FWCore.ParameterSet.Config as cms

pythiaUESettingsBlock = cms.PSet(
        pythiaUESettings = cms.vstring(
                'MSTU(21)=1     ! Check on possible errors during program execution',
                'MSTJ(22)=2     ! Decay those unstable particles',
                'PARJ(71)=10 .  ! for which ctau  10 mm',
                'MSTP(33)=0     ! no K factors in hard cross sections',
                'MSTP(2)=1      ! which order running alphaS',
                'MSTP(51)=10042 ! structure function chosen (external PDF CTEQ6L1)',
                'MSTP(52)=2     ! work with LHAPDF',

                'PARP(82)=1.921 ! pt cutoff for multiparton interactions',
                'PARP(89)=1800. ! sqrts for which PARP82 is set',
                'PARP(90)=0.227 ! Multiple interactions: rescaling power',

                'MSTP(95)=6     ! CR (color reconnection parameters)',
                'PARP(77)=1.016 ! CR',
                'PARP(78)=0.538 ! CR',

                'PARP(80)=0.1   ! Prob. colored parton from BBR',

                'PARP(83)=0.356 ! Multiple interactions: matter distribution parameter',
                'PARP(84)=0.651 ! Multiple interactions: matter distribution parameter',

                'PARP(62)=1.025 ! ISR cutoff',

                'MSTP(91)=1     ! Gaussian primordial kT',
                'PARP(93)=10.0  ! primordial kT-max',

                'MSTP(81)=21    ! multiple parton interactions 1 is Pythia default',
                'MSTP(82)=4     ! Defines the multi-parton model',
        )
)

generator = cms.EDFilter('Pythia6GeneratorFilter',
	comEnergy = cms.double(7000.0),
	crossSection = cms.untracked.double(8.159128e+08),
	filterEfficiency = cms.untracked.double(1),
	maxEventsToPrint = cms.untracked.int32(0),
	pythiaHepMCVerbosity = cms.untracked.bool(False),
	pythiaPylistVerbosity = cms.untracked.int32(0),

	PythiaParameters = cms.PSet(
		pythiaUESettingsBlock,
		processParameters = cms.vstring(
			'MSEL = 1        ! QCD hight pT processes',
			'CKIN(3) = 80    ! minimum pt hat for hard interactions',
			'CKIN(4) = -1    ! maximum pt hat for hard interactions',
		),
		parameterSets = cms.vstring(
			'pythiaUESettings',
			'processParameters',
		)
	)
)

from PhysicsTools.HepMCCandAlgos.genParticles_cfi import genParticles
from RecoJets.Configuration.GenJetParticles_cff import genParticlesForJets
from RecoJets.JetProducers.ak5GenJets_cfi import ak5GenJets

jetfilter = cms.EDFilter("EtaPtMinCandViewSelector",
    src = cms.InputTag("ak5GenJets"),                        
    ptMin = cms.double( 15.0 ),                              
    etaMin = cms.double( 3.0),                              
    etaMax = cms.double( 5.0 )                              
)        
                                                    
Filter = cms.EDFilter("CandViewCountFilter",        
    src = cms.InputTag("jetfilter"),                        
    minNumber = cms.uint32(1)                                
)              

ProductionFilterSequence = cms.Sequence(generator*genParticles*genParticlesForJets*ak5GenJets*jetfilter*Filter)

configurationMetadata = cms.untracked.PSet(
	version = cms.untracked.string('\$Revision: 1.9 $'),
	name = cms.untracked.string('\$Source: /local/reps/CMSSW/CMSSW/Configuration/GenProduction/python/Attic/QCD_Pt_150toInf_fwdJet_TuneZ2star_7TeV_pythia6_cff.py,v $'),
	annotation = cms.untracked.string('LowPU2010 sample with PYTHIA6: QCD dijet production, fwd jet preselection, pThat = 80 .. inf GeV, TuneZ2star')
)
