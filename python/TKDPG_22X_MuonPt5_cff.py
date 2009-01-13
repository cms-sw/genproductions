import FWCore.ParameterSet.Config as cms

source = cms.Source("PythiaSource",
    pythiaPylistVerbosity = cms.untracked.int32(0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    # sqrt(s) = 10 TeV
    comEnergy = cms.untracked.double(10000.0),
    crossSection = cms.untracked.double(498000000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    PythiaParameters = cms.PSet(
            pythiaUESettings = cms.vstring('MSTJ(11)=3     ! Choice of the fragmentation function', 
                'MSTJ(22)=2     ! Decay those unstable particles', 
                'PARJ(71)=10 .  ! for which ctau  10 mm', 
                'MSTP(2)=1      ! which order running alphaS', 
                'MSTP(33)=0     ! no K factors in hard cross sections', 
                'MSTP(51)=7     ! structure function chosen', 
                'MSTP(81)=1     ! multiple parton interactions 1 is Pythia default', 
                'MSTP(82)=4     ! Defines the multi-parton model', 
                'MSTU(21)=1     ! Check on possible errors during program execution', 
                'PARP(82)=1.9409   ! pt cutoff for multiparton interactions', 
                'PARP(89)=1960. ! sqrts for which PARP82 is set', 
                'PARP(83)=0.5   ! Multiple interactions: matter distrbn parameter', 
                'PARP(84)=0.4   ! Multiple interactions: matter distribution parameter', 
                'PARP(90)=0.16  ! Multiple interactions: rescaling power', 
                'PARP(67)=2.5    ! amount of initial-state radiation', 
                'PARP(85)=1.0  ! gluon prod. mechanism in MI', 
                'PARP(86)=1.0  ! gluon prod. mechanism in MI', 
                'PARP(62)=1.25   ! ', 
                'PARP(64)=0.2    ! ', 
                'MSTP(91)=1     !', 
                'PARP(91)=2.1   ! kt distribution', 
                'PARP(93)=15.0  ! '),
		 processParameters = cms.vstring('MSEL=1                 ! QCD high pT processes', 
            'CKIN(3)=20.          ! minimum pt hat for hard interactions'),
        parameterSets = cms.vstring('pythiaUESettings', 
            'processParameters')
        
    )
)

generatorFilter = cms.EDFilter("PythiaFilter",
    MaxEta = cms.untracked.double(2.5),
    MinEta = cms.untracked.double(-2.5),
    MinPt = cms.untracked.double(5.0),
    ParticleID = cms.untracked.int32(13)
)

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /local/projects/CMSSW/rep/CMSSW/Configuration/GenProduction/python/TKDPG_22X_MuonPt5_cff.py,v $'),
    annotation = cms.untracked.string('b/c -> mu pT > 11 GEN evts for SUMMER08')
)
# canonical name needed for further steps
ProductionFilterSequence = cms.Sequence(generatorFilter)
