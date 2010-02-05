# Auto generated configuration file
# using: 
# Revision: 1.155 
# Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/PyReleaseValidation/python/ConfigBuilder.py,v 
# with command line options: Configuration/GenProduction/python/PYTHIA6_MinBias_900GeV_cff.py -s GEN,SIM,DIGI,L1,DIGI2RAW,HLT:GRun -n 10 --conditions STARTUP3X_V8K::All --datatier GEN-SIM-RAW --eventcontent RAWSIM --beamspot Early900GeVCollision --no_exec --customise SimCalorimetry/Configuration/SimCalorimetry_EcalSelectiveReadoutProducers_setBeamcom09_cff.py
import FWCore.ParameterSet.Config as cms

process = cms.Process('HLT')

# import of standard configurations
process.load('Configuration.StandardSequences.Services_cff')
process.load('FWCore.MessageService.MessageLogger_cfi')
process.load('Configuration.StandardSequences.MixingNoPileUp_cff')
process.load('Configuration.StandardSequences.GeometryExtended_cff')
process.load('Configuration.StandardSequences.MagneticField_38T_cff')
process.load('Configuration.StandardSequences.Generator_cff')
process.load('Configuration.StandardSequences.VtxSmearedEarly900GeVCollision_cff')
process.load('Configuration.StandardSequences.Sim_cff')
process.load('Configuration.StandardSequences.Digi_cff')
process.load('Configuration.StandardSequences.SimL1Emulator_cff')
process.load('Configuration.StandardSequences.DigiToRaw_cff')
process.load('HLTrigger.Configuration.HLT_GRun_cff')
process.load('Configuration.StandardSequences.EndOfProcess_cff')
process.load('Configuration.StandardSequences.FrontierConditions_GlobalTag_cff')
process.load('Configuration.EventContent.EventContent_cff')

process.configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    annotation = cms.untracked.string('PYTHIA6-MinBias at 900GeV'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_MinBiasProQ20_900GeV_Summer09-STARTUP3X_V8P_900GeV.py,v $')
)
process.maxEvents = cms.untracked.PSet(
    input = cms.untracked.int32(10)
)
process.options = cms.untracked.PSet(
    Rethrow = cms.untracked.vstring('OtherCMS', 
        'StdException', 
        'Unknown', 
        'BadAlloc', 
        'BadExceptionType', 
        'ProductNotFound', 
        'DictionaryNotFound', 
        'InsertFailure', 
        'Configuration', 
        'LogicError', 
        'UnimplementedFeature', 
        'InvalidReference', 
        'NullPointerError', 
        'NoProductSpecified', 
        'EventTimeout', 
        'EventCorruption', 
        'ScheduleExecutionFailure', 
        'EventProcessorFailure', 
        'FileInPathError', 
        'FileOpenError', 
        'FileReadError', 
        'FatalRootError', 
        'MismatchedInputFiles', 
        'ProductDoesNotSupportViews', 
        'ProductDoesNotSupportPtr', 
        'NotFound')
)
# Input source
process.source = cms.Source("EmptySource")

# Output definition
process.output = cms.OutputModule("PoolOutputModule",
    splitLevel = cms.untracked.int32(0),
    outputCommands = process.RAWSIMEventContent.outputCommands,
    fileName = cms.untracked.string('PYTHIA6_MinBiasProQ20_900GeV_cff_py_GEN_SIM_DIGI_L1_DIGI2RAW_HLT.root'),
    dataset = cms.untracked.PSet(
        dataTier = cms.untracked.string('GEN-SIM-RAW'),
        filterName = cms.untracked.string('')
    ),
    SelectEvents = cms.untracked.PSet(
        SelectEvents = cms.vstring('generation_step')
    )
)

# Additional output definition

# Other statements
process.GlobalTag.globaltag = 'STARTUP3X_V8P::All'
process.generator = cms.EDFilter("Pythia6GeneratorFilter",
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(900.0),
    crossSection = cms.untracked.double(52330000000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    PythiaParameters = cms.PSet(
        pythiaUESettings = cms.vstring('MSTU(21)=1     ! Check on possible errors during program execution', 
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
            'PARP(93)=5.0   ! BR'),
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
        parameterSets = cms.vstring('pythiaUESettings', 
            'processParameters')
    )
)
process.ProductionFilterSequence = cms.Sequence(process.generator)

# shifted/smeared vertex according to data taking

from IOMC.EventVertexGenerators.VtxSmearedParameters_cfi import *
process.VtxSmeared = cms.EDFilter("BetafuncEvtVtxGenerator",
    VtxSmearedCommon,
    Phi = cms.double(0.0),
    BetaStar = cms.double(1100.0),
    Emittance = cms.double(7.82e-07),
    Alpha = cms.double(0.0),
    SigmaZ = cms.double(3.91833),
    TimeOffset = cms.double(0.0),
    X0 = cms.double(0.2542),
    Y0 = cms.double(0.4082),
    Z0 = cms.double(-0.6569)
)

# Path and EndPath definitions
process.generation_step = cms.Path(process.pgen)
process.simulation_step = cms.Path(process.psim)
process.digitisation_step = cms.Path(process.pdigi)
process.L1simulation_step = cms.Path(process.SimL1Emulator)
process.digi2raw_step = cms.Path(process.DigiToRaw)
process.endjob_step = cms.Path(process.endOfProcess)
process.out_step = cms.EndPath(process.output)

# Schedule definition
process.schedule = cms.Schedule(process.generation_step,process.simulation_step,process.digitisation_step,process.L1simulation_step,process.digi2raw_step)
process.schedule.extend(process.HLTSchedule)
process.schedule.extend([process.endjob_step,process.out_step])
# special treatment in case of production filter sequence  
for path in process.paths: 
    getattr(process,path)._seq = process.ProductionFilterSequence*getattr(process,path)._seq


# Automatic addition of the customisation function

def customise(process):

    # Signal in Deconvolution Mode
    process.simSiStripDigis.APVpeakmode = cms.bool(True)
    process.simSiStripDigis.electronPerAdc = cms.double(262.0) #this is the value measured in peak... should we add 15%?

    # ECAL TPG with 2009 beam commissioning TTF thresholds

    process.EcalTrigPrimESProducer.DatabaseFile = cms.untracked.string('TPG_beamv0_MC.txt.gz')
    
    return(process)



# End of customisation function definition

process = customise(process)
