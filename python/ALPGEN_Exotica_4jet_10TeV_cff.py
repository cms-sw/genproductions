import FWCore.ParameterSet.Config as cms

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.3 $'),
    annotation = cms.untracked.string('default documentation string for ALPGEN_Exotica_4jet_10TeV_cff.py'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/ALPGEN_Exotica_4jet_10TeV_cff_py_GEN_IDEAL.py,v $')
)
source = cms.Source("AlpgenSource",
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    fileNames = cms.untracked.vstring('file:4j_0'),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.untracked.double(10000.0),
    GeneratorParameters = cms.PSet(
        parameterSets = cms.vstring('generator'),
        generator = cms.vstring(
	    'IXpar(2) =  1           ! in/ex-clusive: 0/1', 
            'RXpar(1) = 20.          ! minimum cluster Et', 
            'RXpar(2) =  0.4         ! minimum cluster dR'
	)
    ),
    crossSection = cms.untracked.double(54770.0),
    maxEventsToPrint = cms.untracked.int32(1),
    PythiaParameters = cms.PSet(
        parameterSets = cms.vstring('pythia'),
        pythia = cms.vstring(
	    'MSEL=0          ! (D=1)', 
            'MSTJ(11)=3      ! Choice of the fragmentation function', 
            'MSTP(143)=1     ! Call the matching routine in ALPGEN', 
            'MSTJ(22)=2      ! Decay those unstable particles', 
            'PARJ(71)=10.    ! for which ctau 10 mm', 
            'MSTP(2)=1       ! which order running alphaS', 
            'MSTP(33)=0      ! no K factors in hard cross sections', 
            'MSTP(51)=10042  ! CTEQ6L1 structure function chosen', 
            'MSTP(52)=2      ! work with LHAPDF', 
            'MSTP(81)=1      ! multiple parton interactions 1 is Pythia default', 
            'MSTP(82)=4      ! Defines the multi-parton model', 
            'MSTU(21)=1      ! Check on possible errors during program execution', 
            'PARP(82)=1.8387 ! pt cutoff for multiparton interactions', 
            'PARP(89)=1960.  ! sqrts for which PARP82 is set', 
            'PARP(83)=0.5    ! Multiple interactions: matter distrbn parameter', 
            'PARP(84)=0.4    ! Multiple interactions: matter distribution parameter', 
            'PARP(90)=0.16   ! Multiple interactions: rescaling power', 
            'PARP(67)=2.5    ! amount of initial-state radiation', 
            'PARP(85)=1.0    ! gluon prod. mechanism in MI', 
            'PARP(86)=1.0    ! gluon prod. mechanism in MI', 
            'PARP(62)=1.25   ! ', 
            'PARP(64)=0.2    ! ', 
            'MSTP(91)=1      ! ', 
            'PARP(91)=2.1    ! kt distribution', 
            'PARP(93)=15.0   ! '
	)
    )
)
filter = cms.EDFilter("AlpgenEmptyEventFilter")
ProductionFilterSequence = cms.Sequence(filter)
