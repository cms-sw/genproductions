import FWCore.ParameterSet.Config as cms
                                
source = cms.Source("EmptySource")
generator = cms.EDFilter("Pythia6GeneratorFilter",
    pythiaPylistVerbosity = cms.untracked.int32(0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    crossSection = cms.untracked.double(21150000000.0),
    filterEfficiency = cms.untracked.double(0.000096),
    maxEventsToPrint = cms.untracked.int32(0),
    PythiaParameters = cms.PSet(
         pythiaUESettings = cms.vstring(
        'MSTU(21)=1     ! Check on possible errors during program execution', 
        'MSTJ(22)=2     ! Decay those unstable particles', 
        'PARJ(71)=10 .  ! for which ctau  10 mm', 
        'MSTP(2)=1      ! which order running alphaS', 
        'MSTP(33)=0     ! no K factors in hard cross sections', 
        'MSTP(51)=7     ! structure function chosen (internal PDF CTEQ5L)',
	'MSTP(52)=1     ! work with LHAPDF',
	'PARJ(1)=0.073  ! FLAV (Tuned by Professor on LEP data)',
	'PARJ(2)=0.2    ! FLAV (Tuned by Professor on LEP data)',
	'PARJ(3)=0.94   ! FLAV (Tuned by Professor on LEP data)',
	'PARJ(4)=0.032  ! FLAV (Tuned by Professor on LEP data)',
	'PARJ(11)=0.31  ! FLAV (Tuned by Professor on LEP data)',
	'PARJ(12)=0.4   ! FLAV (Tuned by Professor on LEP data)',
	'PARJ(13)=0.54  ! FLAV (Tuned by Professor on LEP data)',
	'PARJ(25)=0.63  ! FLAV (Tuned by Professor on LEP data)',
	'PARJ(26)=0.12  ! FLAV (Tuned by Professor on LEP data)',
        'MSTJ(11)=5     ! HAD Choice of the fragmentation function',    
	'PARJ(21)=0.313 ! HAD (Tuned by Professor on LEP data)', 
        'PARJ(41)=0.49  ! HAD (Tuned by Professor on LEP data)',                                     
        'PARJ(42)=1.2   ! HAD (Tuned by Professor on LEP data)',                                     
        'PARJ(46)=1.0   ! HAD (Tuned by Professor on LEP data)',                                     
        'PARJ(47)=1.0   ! HAD (Tuned by Professor on LEP data)',                                     
        'PARP(62)=2.9   ! ISR', 
        'PARP(64)=0.14  ! ISR', 
        'PARP(67)=2.65  ! ISR', 
        'MSTP(81)=1     ! MPI 1 is old Pythia set of models', 
        'MSTP(82)=4     ! MPI model', 
        'PARP(82)=1.9   ! MPI pt cutoff for multiparton interactions', 
        'PARP(83)=0.83  ! MPI matter distrbn parameter', 
        'PARP(84)=0.6   ! MPI matter distribution parameter', 
        'PARP(85)=0.86  ! MPI gluon prod. mechanism', 
        'PARP(86)=0.93  ! MPI gluon prod. mechanism', 
        'PARP(89)=1800. ! MPI sqrts for which PARP82 is set', 
        'PARP(90)=0.22  ! MPI rescaling power', 
        'MSTP(91)=1     ! BR', 
        'PARP(91)=2.1   ! BR kt distribution', 
        'PARP(93)=5.0   ! BR'
        ),
        processParameters = cms.vstring('MSEL=1           ! User defined processes', 
            'MSTJ(22)=4       ! Decay unstable particles in a cylinder', 
            'PARJ(73)=1500.   ! max. radius for MSTJ(22)=4', 
            'PARJ(74)=3000.   ! max. Z for MSTJ(22)=4', 
            'MDCY(C130,1)=1   ! decay k0-longs', 
            'MDCY(C211,1)=1   ! decay pions', 
            'MDCY(C321,1)=1   ! decay kaons',
	    'CKIN(3)=6        ! pthat min'),
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

