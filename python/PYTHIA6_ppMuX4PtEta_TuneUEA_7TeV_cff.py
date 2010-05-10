import FWCore.ParameterSet.Config as cms
                               
source = cms.Source("EmptySource")
generator = cms.EDFilter("Pythia6GeneratorFilter",
    pythiaPylistVerbosity = cms.untracked.int32(0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    crossSection = cms.untracked.double(21150000000.0),
    filterEfficiency = cms.untracked.double(0.00096),
    maxEventsToPrint = cms.untracked.int32(0),
    PythiaParameters = cms.PSet(
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
        'PARP(82)=1.8   ! pt cutoff for multiparton interactions', 
        'PARP(89)=1000. ! sqrts for which PARP82 is set', 
        'PARP(83)=0.5   ! Multiple interactions: matter distrbn parameter', 
        'PARP(84)=0.4   ! Multiple interactions: matter distribution parameter', 
        'PARP(90)=0.19  ! Multiple interactions: rescaling power', 
        'PARP(67)=1.0    ! amount of initial-state radiation', 
        'PARP(85)=0.33  ! gluon prod. mechanism in MI', 
        'PARP(86)=0.66  ! gluon prod. mechanism in MI', 
        'PARP(62)=1.0    ! ', 
        'PARP(64)=1.0    ! ', 
        'MSTP(91)=1      !', 
        'PARP(91)=1.0   ! kt distribution', 
        'PARP(93)=5.0  ! '
    	),
        processParameters = cms.vstring('MSEL=1           ! User defined processes', 
            'MSTJ(22)=4       ! Decay unstable particles in a cylinder', 
            'PARJ(73)=1500.   ! max. radius for MSTJ(22)=4', 
            'PARJ(74)=3000.   ! max. Z for MSTJ(22)=4', 
            'MDCY(C130,1)=1   ! decay k0-longs', 
            'MDCY(C211,1)=1   ! decay pions', 
            'MDCY(C321,1)=1   ! decay kaons',
	    'CKIN(3)=6        ! pthat min '),
        # This is a vector of ParameterSet names to be read, in this order
        parameterSets = cms.vstring('pythiaUESettings',
            'processParameters')
    )
)

mugenfilter = cms.EDFilter("MCSmartSingleParticleFilter",
    Status = cms.untracked.vint32(     1,    1),
    ParticleID = cms.untracked.vint32(13,  -13),
    MinPt = cms.untracked.vdouble(    4, 4),
    MaxEta = cms.untracked.vdouble(   2.1, 2.1),
    MinEta = cms.untracked.vdouble(  -2.1, -2.1),
    MaxDecayRadius = cms.untracked.vdouble(2000.0, 2000.0),
    MaxDecayZ = cms.untracked.vdouble(4000.0, 4000.0),
    MinDecayZ = cms.untracked.vdouble(-4000.0, -4000.0)
)

ProductionFilterSequence = cms.Sequence(generator*mugenfilter)

