# Auto generated configuration file
# using: 
# Revision: 1.77.2.1 
# Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/PyReleaseValidation/python/ConfigBuilder.py,v 
# with command line options: Configuration/GenProduction/python/PYTHIA6_Tauola_MSSM_Hch_tau_mA161_10TeV_cff.py -s GEN --eventcontent RAWSIM --datatier GEN --conditions FrontierConditions_GlobalTag,IDEAL_V9::All -n 1000 --no_exec
import FWCore.ParameterSet.Config as cms

process = cms.Process('GEN')

# import of standard configurations
process.load('Configuration/StandardSequences/Services_cff')
process.load('FWCore/MessageService/MessageLogger_cfi')
process.load('Configuration/StandardSequences/MixingNoPileUp_cff')
process.load('Configuration/StandardSequences/GeometryPilot2_cff')
process.load('Configuration/StandardSequences/MagneticField_38T_cff')
process.load('Configuration/StandardSequences/Generator_cff')
process.load('Configuration/StandardSequences/VtxSmearedEarly10TeVCollision_cff')
process.load('Configuration/StandardSequences/FrontierConditions_GlobalTag_cff')
process.load('Configuration/EventContent/EventContent_cff')

process.configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    annotation = cms.untracked.string('PYTHIA6-Hch->taunu mA=200GeV with TAUOLA at 10TeV'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_Tauola_MSSM_Hch_tau_mA161_10TeV_cff.py,v $')
)
process.maxEvents = cms.untracked.PSet(
    input = cms.untracked.int32(1000)
)
process.options = cms.untracked.PSet(
    Rethrow = cms.untracked.vstring('ProductNotFound')
)
# Input source
process.source = cms.Source("PythiaSource",
    UseExternalGenerators = cms.untracked.bool(True),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    ExternalGenerators = cms.PSet(
        Tauola = cms.untracked.PSet(
            UseTauolaPolarization = cms.bool(True),
            InputCards = cms.vstring('TAUOLA = 0 0 0   ! TAUOLA ')
        ),
        parameterSets = cms.vstring('Tauola')
    ),
    comEnergy = cms.untracked.double(10000.0),
    crossSection = cms.untracked.double(55000000000.0),
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
        processParameters = cms.vstring('MSEL      = 0     ! User defined processes', 
            'MSUB(401) = 1     ! gg->tbH+', 
            'MSUB(402) = 1     ! qq->tbH+', 
            'IMSS(1)   = 1     ! MSSM', 
            'RMSS(5)   = 30.   ! TANBETA', 
            'RMSS(19)  = 161.  ! (D=850.) m_A', 
            'MDME(503,1)=0           !Higgs(H+) decay into dbar            u', 
            'MDME(504,1)=0           !Higgs(H+) decay into sbar            c', 
            'MDME(505,1)=0           !Higgs(H+) decay into bbar            t', 
            'MDME(506,1)=0           !Higgs(H+) decay into b bar           t', 
            'MDME(507,1)=0           !Higgs(H+) decay into e+              nu_e', 
            'MDME(508,1)=0           !Higgs(H+) decay into mu+             nu_mu', 
            'MDME(509,1)=1           !Higgs(H+) decay into tau+            nu_tau', 
            'MDME(510,1)=0           !Higgs(H+) decay into tau prime+      nu_tau', 
            'MDME(511,1)=0           !Higgs(H+) decay into W+              h0', 
            'MDME(512,1)=0           !Higgs(H+) decay into ~chi_10         ~chi_1+', 
            'MDME(513,1)=0           !Higgs(H+) decay into ~chi_10         ~chi_2+', 
            'MDME(514,1)=0           !Higgs(H+) decay into ~chi_20         ~chi_1+', 
            'MDME(515,1)=0           !Higgs(H+) decay into ~chi_20         ~chi_2+', 
            'MDME(516,1)=0           !Higgs(H+) decay into ~chi_30         ~chi_1+', 
            'MDME(517,1)=0           !Higgs(H+) decay into ~chi_30         ~chi_2+', 
            'MDME(518,1)=0           !Higgs(H+) decay into ~chi_40         ~chi_1+', 
            'MDME(519,1)=0           !Higgs(H+) decay into ~chi_40         ~chi_2+', 
            'MDME(520,1)=0           !Higgs(H+) decay into ~t_1            ~b_1bar', 
            'MDME(521,1)=0           !Higgs(H+) decay into ~t_2            ~b_1bar', 
            'MDME(522,1)=0           !Higgs(H+) decay into ~t_1            ~b_2bar', 
            'MDME(523,1)=0           !Higgs(H+) decay into ~t_2            ~b_2bar', 
            'MDME(524,1)=0           !Higgs(H+) decay into ~d_Lbar         ~u_L', 
            'MDME(525,1)=0           !Higgs(H+) decay into ~s_Lbar         ~c_L', 
            'MDME(526,1)=0           !Higgs(H+) decay into ~e_L+           ~nu_eL', 
            'MDME(527,1)=0           !Higgs(H+) decay into ~mu_L+          ~nu_muL', 
            'MDME(528,1)=0           !Higgs(H+) decay into ~tau_1+         ~nu_tauL', 
            'MDME(529,1)=0           !Higgs(H+) decay into ~tau_2+         ~nu_tauL'),
        parameterSets = cms.vstring('pythiaUESettings', 
            'processParameters')
    )
)

# Output definition
process.output = cms.OutputModule("PoolOutputModule",
    outputCommands = process.RAWSIMEventContent.outputCommands,
    fileName = cms.untracked.string('PYTHIA6_Tauola_MSSM_Hch_tau_mA161_10TeV_cff_py_GEN.root'),
    dataset = cms.untracked.PSet(
        dataTier = cms.untracked.string('GEN'),
        filterName = cms.untracked.string('')
    ),
    SelectEvents = cms.untracked.PSet(
        SelectEvents = cms.vstring('generation_step')
    )
)

# Additional output definition

# Other statements
process.GlobalTag.globaltag = 'IDEAL_V9::All'

# Path and EndPath definitions
process.generation_step = cms.Path(process.pgen)
process.out_step = cms.EndPath(process.output)

# Schedule definition
process.schedule = cms.Schedule(process.generation_step,process.out_step)
