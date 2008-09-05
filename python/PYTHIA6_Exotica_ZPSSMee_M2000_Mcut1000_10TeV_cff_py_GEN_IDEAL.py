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
    version = cms.untracked.string('$Revision: 1.1 $'),
    annotation = cms.untracked.string("1 TeV Z\' SSM  at sqrt{s} = 10 TeV"),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_Exotica_ZPSSMee_M2000_Mcut1000_10TeV_cff_py_GEN_IDEAL.py,v $')
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
    crossSection = cms.untracked.double(0.007856),
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
        processParameters = cms.vstring('MSEL=0             ! User defined processes', 
            "MSUB(141)   = 1    ! ff -> gamma/Z0/Z\'", 
            'MSTP(44)    = 7    ! complete Zprime/Z/gamma interference', 
            "PMAS(32,1)  = 2000 ! Z\' mass (GeV)", 
            'CKIN(1)     = 1000  ! lower invariant mass cutoff (GeV)', 
            'CKIN(2)     = -1   ! no upper invariant mass cutoff', 
            'MDME(289,1) = 0    ! d dbar', 
            'MDME(290,1) = 0    ! u ubar', 
            'MDME(291,1) = 0    ! s sbar', 
            'MDME(292,1) = 0    ! c cbar', 
            'MDME(293,1) = 0    ! b bar', 
            'MDME(294,1) = 0    ! t tbar', 
            'MDME(295,1) = -1   ! 4th gen q qbar', 
            'MDME(296,1) = -1   ! 4th gen q qbar', 
            'MDME(297,1) = 1    ! e-     e+', 
            'MDME(298,1) = 0    ! nu_e   nu_ebar', 
            'MDME(299,1) = 0    ! mu-    mu+', 
            'MDME(300,1) = 0    ! nu_mu  nu_mubar', 
            'MDME(301,1) = 0    ! tau    tau', 
            'MDME(302,1) = 0    ! nu_tau nu_taubar', 
            'MDME(303,1) = -1   ! 4th gen l- l+', 
            'MDME(304,1) = -1   ! 4th gen nu nubar', 
            'MDME(305,1) = -1   ! W+ W-', 
            'MDME(306,1) = -1   ! H+ H-', 
            'MDME(307,1) = -1   ! Z0 gamma', 
            'MDME(308,1) = -1   ! Z0 h0', 
            'MDME(309,1) = -1   ! h0 A0', 
            'MDME(310,1) = -1   ! H0 A0'),
        parameterSets = cms.vstring('pythiaUESettings', 
            'processParameters')
    )
)

# Output definition
process.output = cms.OutputModule("PoolOutputModule",
    outputCommands = process.RAWSIMEventContent.outputCommands,
    fileName = cms.untracked.string('PYTHIA6_Exotica_ZPSSMee_M2000_Mcut800_10TeV_cff_py_GEN.root'),
    dataset = cms.untracked.PSet(
        dataTier = cms.untracked.string('GEN'),
        filterName = cms.untracked.string('')
    ),
    SelectEvents = cms.untracked.PSet(
        SelectEvents = cms.vstring('generation_step')
    )
)

# Other statements
process.GlobalTag.globaltag = 'IDEAL_V9::All'

# Path and EndPath definitions
process.generation_step = cms.Path(process.pgen)
process.out_step = cms.EndPath(process.output)

# Schedule definition
process.schedule = cms.Schedule(process.generation_step,process.out_step)
