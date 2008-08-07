# Auto generated configuration file
# using: 
# Revision: 1.57 
# Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/PyReleaseValidation/python/ConfigBuilder.py,v 
import FWCore.ParameterSet.Config as cms

process = cms.Process('GEN')

# import of standard configurations
process.load('Configuration/StandardSequences/Services_cff')
process.load('FWCore/MessageService/MessageLogger_cfi')
process.load('Configuration/StandardSequences/Generator_cff')
process.load('Configuration/StandardSequences/MixingNoPileUp_cff')
process.load('Configuration/StandardSequences/GeometryPilot2_cff')
process.load('Configuration/StandardSequences/MagneticField_cff')
process.load('Configuration/StandardSequences/Generator_cff')
process.load('Configuration/StandardSequences/VtxSmearedEarly10TeVCollision_cff')
process.load('Configuration/StandardSequences/FrontierConditions_GlobalTag_cff')
process.load('Configuration/EventContent/EventContent_cff')

process.configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.3 $'),
    annotation = cms.untracked.string('generation of prompt Upsilon 2S COM+CSM'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_Upsilon2S_1S_10TeV_cff.py,v $')
)
process.maxEvents = cms.untracked.PSet(
    input = cms.untracked.int32(10)
)
process.options = cms.untracked.PSet(
    Rethrow = cms.untracked.vstring('ProductNotFound')
)
# Input source
process.source = cms.Source("PythiaSource",
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(0.0575),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.untracked.double(10000.0),
    crossSection = cms.untracked.double(820.0),
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
        processParameters = cms.vstring('MSEL=62               ! Quarkonia', 
            'KFPR(461,1)  = 100553     ! change 461 to Upsilon(2S) + g', 
            'PMAS(365,1)  = 10.0300   ! change bb~ mass larger than Upsilon(2S) 10.02330', 
            'PMAS(366,1)  = 10.0300   ! change bb~ mass larger than Upsilon(2S) 10.02330', 
            'PMAS(367,1)  = 10.0300   ! change bb~ mass larger than Upsilon(2S) 10.02330', 
            'KFDP(4214,1) = 100553     ! bb~ -> Upsilon(2S)', 
            'KFDP(4215,1) = 100553     ! bb~ -> Upsilon(2S)', 
            'KFDP(4216,1) = 100553     ! bb~ -> Upsilon(2S)', 
            'PMAS(278,1)  = 10.23250   ! change chi_0b(1P) mass to chi_0b(2P)', 
            'KFDP(1520,1) = 100553     ! chi_0b(2P) -> Upsilon(2S)', 
            'BRAT(1520)   = 0.046      ! br of chi_0b(2P) -> Upsilon(2S)', 
            'BRAT(1521)   = 0.954      ! br of chi_0b(2P) -> rndmflav rndmflavbar', 
            'PMAS(294,1)  = 10.25546   ! change chi_1b(1P) mass to chi_1b(2P)', 
            'KFDP(1565,1) = 100553     ! chi_1b(2P) -> Upsilon(2S)', 
            'BRAT(1565)   = 0.210      ! br of chi_1b(2P) -> Upsilon(2S)', 
            'BRAT(1566)   = 0.790      ! br of chi_1b(2P) -> rndmflav rndmflavbar', 
            'PMAS(148,1)  = 10.26865   ! change chi_2b(1P) mass to chi_2b(2P)', 
            'KFDP(1043,1) = 100553     ! chi_2b(2P) -> Upsilon(2S)', 
            'BRAT(1043)   = 0.162      ! br of chi_2b(2P) -> Upsilon(2S)', 
            'BRAT(1044)   = 0.838      ! br of chi_2b(2P) -> rndmflav rndmflavbar', 
            'PARP(146)=4.63   ! New values for COM matrix elements', 
            'PARP(147)=0.045  ! New values for COM matrix elements', 
            'PARP(148)=0.006  ! New values for COM matrix elements', 
            'PARP(149)=0.006  ! New values for COM matrix elements', 
            'PARP(150)=0.108  ! New values for COM matrix elements', 
            'MDME(1578,1) = 0 ! 0.014000    e-              e+', 
            'MDME(1579,1) = 0 ! 0.014000    mu-             mu+', 
            'MDME(1580,1) = 0 ! 0.014000    tau-            tau+', 
            'MDME(1581,1) = 0 ! 0.008000    d               dbar', 
            'MDME(1582,1) = 0 ! 0.024000    u               ubar', 
            'MDME(1583,1) = 0 ! 0.008000    s               sbar', 
            'MDME(1584,1) = 0 ! 0.024000    c               cbar', 
            'MDME(1585,1) = 0 ! 0.425000    g               g            g', 
            'MDME(1586,1) = 0 ! 0.020000    gamma           g            g', 
            'MDME(1587,1) = 1 ! 0.185000    Upsilon         pi+          pi-', 
            'MDME(1588,1) = 1 ! 0.088000    Upsilon         pi0          pi0', 
            'MDME(1589,1) = 0 ! 0.043000    chi_0b          gamma', 
            'MDME(1590,1) = 0 ! 0.067000    chi_1b          gamma', 
            'MDME(1591,1) = 0 ! 0.066000    chi_2b          gamma', 
            'MDME(1034,1)=0   ! 0.025200    e- e+', 
            'MDME(1035,1)=1   ! 0.024800    mu- mu+', 
            'MDME(1036,1)=0   ! 0.026700    tau- tau+', 
            'MDME(1037,1)=0   ! 0.015000    d dbar', 
            'MDME(1038,1)=0   ! 0.045000    u ubar', 
            'MDME(1039,1)=0   ! 0.015000    s sbar', 
            'MDME(1040,1)=0   ! 0.045000    c cbar', 
            'MDME(1041,1)=0   ! 0.774300    g g g', 
            'MDME(1042,1)=0   ! 0.029000    gamma g', 
            'MSTP(142)=2      ! turns on the PYEVWT Pt re-weighting routine', 
            'PARJ(13)=0.750   ! probability that a c or b meson has S=1', 
            'PARJ(14)=0.162   ! probability that a meson with S=0 is produced with L=1, J=1', 
            'PARJ(15)=0.018   ! probability that a meson with S=1 is produced with L=1, J=0', 
            'PARJ(16)=0.054   ! probability that a meson with S=1 is produced with L=1, J=1', 
            'MSTP(145)=0      ! choice of polarization', 
            'MSTP(146)=0      ! choice of polarization frame ONLY when mstp(145)=1', 
            'MSTP(147)=0      ! particular helicity or density matrix component when mstp(145)=1', 
            'MSTP(148)=1      ! possibility to allow for final-state shower evolution, extreme case !', 
            'MSTP(149)=1      ! if mstp(148)=1, it determines the kinematics of the QQ~3S1(8)->QQ~3S1(8)+g branching'),
        parameterSets = cms.vstring('pythiaUESettings', 
            'processParameters', 
            'CSAParameters'),
        CSAParameters = cms.vstring('CSAMODE = 6     ! cross-section reweighted quarkonia')
    )
)

# Output definition
process.output = cms.OutputModule("PoolOutputModule",
    outputCommands = process.RAWSIMEventContent.outputCommands,
    fileName = cms.untracked.string('PYTHIA6_Upsilon2S_1S_10TeV_cff_py_GEN.root'),
    dataset = cms.untracked.PSet(
        dataTier = cms.untracked.string('GEN'),
        filterName = cms.untracked.string('')
    ),
    SelectEvents = cms.untracked.PSet(
        SelectEvents = cms.vstring('generation_step')
    )
)

# Other statements
process.GlobalTag.globaltag = 'STARTUP_V4::All'
process.oniafilter = cms.EDFilter("PythiaFilter",
    MaxEta = cms.untracked.double(1000.0),
    Status = cms.untracked.int32(2),
    MinEta = cms.untracked.double(-1000.0),
    MinPt = cms.untracked.double(0.0),
    ParticleID = cms.untracked.int32(553)
)
process.mumugenfilter = cms.EDFilter("MCParticlePairFilter",
    Status = cms.untracked.vint32(1, 1),
    MinPt = cms.untracked.vdouble(2.5, 2.5),
    MaxEta = cms.untracked.vdouble(2.5, 2.5),
    MinEta = cms.untracked.vdouble(-2.5, -2.5),
    ParticleCharge = cms.untracked.int32(-1),
    ParticleID1 = cms.untracked.vint32(13),
    ParticleID2 = cms.untracked.vint32(13)
)
process.ProductionFilterSequence = cms.Sequence(process.oniafilter*process.mumugenfilter)

# Path and EndPath definitions
process.generation_step = cms.Path(process.ProductionFilterSequence*process.pgen)
process.out_step = cms.EndPath(process.output)

# Schedule definition
process.schedule = cms.Schedule(process.generation_step,process.out_step)
