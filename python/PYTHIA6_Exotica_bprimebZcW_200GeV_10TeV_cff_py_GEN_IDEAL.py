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
    version = cms.untracked.string('$Revision: 1.7 $'),
    annotation = cms.untracked.string('PYTHIA6-bprimebZcW_200GeV at 10TeV'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_Exotica_bprimebZcW_200GeV_10TeV_cff_GEN_IDEAL.py,v $')
)
process.maxEvents = cms.untracked.PSet(
    input = cms.untracked.int32(1000)
)
process.options = cms.untracked.PSet(
    Rethrow = cms.untracked.vstring('ProductNotFound')
)
# Input source
process.source = cms.Source("PythiaSource",
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.untracked.double(10000.0),
    crossSection = cms.untracked.double(2.840),
    maxEventsToPrint = cms.untracked.int32(0),
    PythiaParameters = cms.PSet(
        pythiaUESettings = cms.vstring('MSTJ(11)=3     ! Choice of the fragmentation function', 
            'MSTJ(22)=2     ! Decay those unstable particles', 
            'PARJ(71)=10 .  ! for which ctau  10 mm', 
            'MSTP(2)=1      ! which order running alphaS', 
            'MSTP(33)=0     ! no K factors in hard cross sections', 
            'MSTP(51)=10042     ! CTEQ6L1 structure function chosen', 
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
            'MSTP(91)=1     !', 
            'PARP(91)=2.1   ! kt distribution', 
            'PARP(93)=15.0  ! '),
        processParameters = cms.vstring('MSTP(1) = 4     ! User defined processes', 
            'MSEL = 7', 
            'MWID(7)=2', 
            'PMAS(7,1) = 200.0D0', 
            'PMAS(7,2) = 1.0D0', 
            'PMAS(7,3) = 10.0D0', 
            'VCKM(1,4) = 0.00078961D0', 
            'VCKM(2,4) = 0.01354896D0', 
            'VCKM(3,4) = 0.04700224D0', 
            'VCKM(4,4) = 0.93857344D0', 
            'VCKM(4,1) = 0.00001936D0', 
            'VCKM(4,2) = 0.01290496D0', 
            'VCKM(4,3) = 0.04840000D0', 
            'MDME(56,1)=0     ! g b4', 
            'MDME(57,1)=0     ! gamma b4', 
            'KFDP(58,2)=5     ! defines Z0 b', 
            'MDME(58,1)=2     ! Z0 b', 
            'MDME(59,1)=0     ! W u', 
            'MDME(60,1)=3     ! W c', 
            'MDME(61,1)=0     ! W t', 
            'MDME(62,1)=0     ! W t4', 
            'MDME(63,1)=0     ! h0 b4', 
            'MDME(64,1)=-1    ! H- c', 
            'MDME(65,1)=-1    ! H- t', 
            'BRAT(56)  = 0.0D0', 
            'BRAT(57)  = 0.0D0', 
            'BRAT(58)  = 0.5D0', 
            'BRAT(59)  = 0.0D0', 
            'BRAT(60)  = 0.5D0', 
            'BRAT(61)  = 0.0D0', 
            'BRAT(62)  = 0.0D0', 
            'BRAT(63)  = 0.0D0', 
            'BRAT(64)  = 0.0D0', 
            'BRAT(65)  = 0.0D0', 
            'MDME(174,1)=0           !Z decay into d dbar', 
            'MDME(175,1)=0           !Z decay into u ubar', 
            'MDME(176,1)=0           !Z decay into s sbar', 
            'MDME(177,1)=0           !Z decay into c cbar', 
            'MDME(178,1)=0           !Z decay into b bbar', 
            'MDME(179,1)=0           !Z decay into t tbar', 
            'MDME(180,1)=-1          !Z decay into b4 b4bar', 
            'MDME(181,1)=-1          !Z decay into t4 t4bar', 
            'MDME(182,1)=1           !Z decay into e- e+', 
            'MDME(183,1)=0           !Z decay into nu_e nu_ebar', 
            'MDME(184,1)=1           !Z decay into mu- mu+', 
            'MDME(185,1)=0           !Z decay into nu_mu nu_mubar', 
            'MDME(186,1)=1           !Z decay into tau- tau+', 
            'MDME(187,1)=0           !Z decay into nu_tau nu_taubar', 
            'MDME(188,1)=-1          !Z decay into tau4 tau4bar', 
            'MDME(189,1)=-1          !Z decay into nu_tau4 nu_tau4bar', 
            'MDME(190,1)=1           !W decay into u dbar', 
            'MDME(191,1)=1           !W decay into c dbar', 
            'MDME(192,1)=1           !W decay into t dbar', 
            'MDME(193,1)=-1          !W decay into t4 dbar', 
            'MDME(202,1)=-1          !W decay into u b4bar', 
            'MDME(203,1)=-1          !W decay into c b4bar', 
            'MDME(204,1)=-1          !W decay into t b4bar', 
            'MDME(205,1)=-1          !W decay into t4 b4bar', 
            'MDME(206,1)=1           !W decay into e- nu_e', 
            'MDME(207,1)=1           !W decay into mu nu_mu', 
            'MDME(208,1)=1           !W decay into tau nu_tau', 
            'MDME(209,1)=-1          !W decay into tau4 nu_tau4'),
        parameterSets = cms.vstring('pythiaUESettings', 
            'processParameters')
    )
)

# Output definition
process.output = cms.OutputModule("PoolOutputModule",
    outputCommands = process.RAWSIMEventContent.outputCommands,
    fileName = cms.untracked.string('PYTHIA6_Exotica_bprimebZcW_200GeV_10TeV_cff_py_GEN.root'),
    dataset = cms.untracked.PSet(
        dataTier = cms.untracked.string('GEN'),
        filterName = cms.untracked.string('')
    ),
    SelectEvents = cms.untracked.PSet(
        SelectEvents = cms.vstring('generation_step')
    )
)

# Other statements
process.GlobalTag.globaltag = 'IDEAL_V6::All'

# Path and EndPath definitions
process.generation_step = cms.Path(process.pgen)
process.out_step = cms.EndPath(process.output)

# Schedule definition
process.schedule = cms.Schedule(process.generation_step,process.out_step)
