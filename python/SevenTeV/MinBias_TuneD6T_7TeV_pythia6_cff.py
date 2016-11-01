import FWCore.ParameterSet.Config as cms

pythiaUESettingsBlock = cms.PSet(
	pythiaUESettings = cms.vstring(
        'MSTJ(11)=3     ! Choice of the fragmentation function', 
        'MSTJ(22)=2     ! Decay those unstable particles', 
        'PARJ(71)=10 .  ! for which ctau  10 mm', 
        'MSTP(2)=1      ! which order running alphaS', 
        'MSTP(33)=0     ! no K factors in hard cross sections', 
        'MSTP(51)=10042 ! structure function chosen (external PDF CTEQ6L1)',
	'MSTP(52)=2     ! work with LHAPDF',
        'MSTP(81)=1     ! multiple parton interactions 1 is Pythia default', 
        'MSTP(82)=4     ! Defines the multi-parton model', 
        'MSTU(21)=1     ! Check on possible errors during program execution', 
        'PARP(82)=1.8387   ! pt cutoff for multiparton interactions', 
        'PARP(89)=1960. ! sqrts for which PARP82 is set', 
        'PARP(83)=0.5   ! Multiple interactions: matter distrbn parameter', 
        'PARP(84)=0.4   ! Multiple interactions: matter distribution parameter', 
        'PARP(90)=0.16  ! Multiple interactions: rescaling power', 
        'PARP(67)=2.5    ! amount of initial-state radiation', 
        'PARP(85)=1.0  ! gluon prod. mechanism in MI', 
        'PARP(86)=1.0  ! gluon prod. mechanism in MI', 
        'PARP(62)=1.25   ! ', 
        'PARP(64)=0.2    ! ', 
        'MSTP(91)=1      !', 
        'PARP(91)=2.1   ! kt distribution', 
        'PARP(93)=15.0  ! '
	)
)

generator = cms.EDFilter("Pythia6GeneratorFilter",
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(1.0),
    crossSection = cms.untracked.double(71260000000.),
    comEnergy = cms.double(7000.0),
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
    name = cms.untracked.string('$Source: /local/reps/CMSSW/CMSSW/Configuration/GenProduction/python/SevenTeV/MinBias_TuneD6T_7TeV_pythia6_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6-MinBias TuneD6T at 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
