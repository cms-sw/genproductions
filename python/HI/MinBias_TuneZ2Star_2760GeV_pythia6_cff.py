import FWCore.ParameterSet.Config as cms

#from Configuration.Generator.HI.PythiaUEZ2starSettings_cfi import *

# this is Z2star
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


generator = cms.EDFilter("Pythia6GeneratorFilter",
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(1.0),
    crossSection = cms.untracked.double(72700000000),
    comEnergy = cms.double(2760.0),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('MSEL=0         ! User defined processes', 
            'MSUB(11)=1     ! Min bias process', 
            'MSUB(12)=1     ! Min bias process', 
            'MSUB(13)=1     ! Min bias process', 
            'MSUB(28)=1     ! Min bias process', 
            'MSUB(53)=1     ! Min bias process', 
            'MSUB(68)=1     ! Min bias process', 
            'MSUB(92)=1     ! Min bias process, single diffractive', 
            'MSUB(93)=1     ! Min bias process, single diffractive', 
            'MSUB(94)=1     ! Min bias process, double diffractive', 
            'MSUB(95)=1     ! Min bias process'),
        # This is a vector of ParameterSet names to be read, in this order
        parameterSets = cms.vstring('pythiaUESettings', 
            'processParameters')
    )
)

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.2 $'),
    name = cms.untracked.string('$Source: /local/reps/CMSSW/CMSSW/Configuration/GenProduction/python/HI/MinBias_TuneZ2Star_2760GeV_pythia6_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6-MinBias TuneZ2star at 8TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
