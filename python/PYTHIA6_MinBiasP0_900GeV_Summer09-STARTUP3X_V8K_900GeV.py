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
    version = cms.untracked.string('$Revision: 1.2 $'),
    annotation = cms.untracked.string('PYTHIA6-MinBias at 900GeV'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6P0_MinBias_900GeV_Summer09-STARTUP3X_V8K_900GeV.py,v $')
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
    fileName = cms.untracked.string('PYTHIA6_MinBiasP0_900GeV_cff_py_GEN_SIM_DIGI_L1_DIGI2RAW_HLT.root'),
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
process.GlobalTag.globaltag = 'STARTUP3X_V8K::All'
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
            'PARJ(71)=10.   ! for which ctau  10 mm', 
            'MSTP(2)=1      ! which order running alphaS', 
            'MSTP(33)=0     ! no K factors in hard cross sections', 
            'MSTP(51)=7     ! structure function chosen (internal PDF CTEQ5L)', 
            'MSTP(52)=1     ! work with internal PDF', 
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
            'PARJ(21)=0.313 ! HAD', 
            'PARJ(41)=0.49  ! HAD', 
            'PARJ(42)=1.2   ! HAD', 
            'PARJ(46)=1.0   ! HAD', 
            'PARJ(47)=1.0   ! HAD', 
            'MSTP(64)=3     ! ISR', 
            'PARP(64)=1.0   ! ISR', 
            'MSTP(67)=2     ! ISR', 
            'PARP(67)=1.0   ! ISR', 
            'MSTP(70)=2     ! ISR', 
            'MSTP(72)=1     ! ISR', 
            'PARJ(81)=0.257 ! FSR', 
            'PARJ(82)=0.8   ! FSR', 
            'PARP(71)=2.0   ! FSR', 
            'MSTP(81)=21    ! MPI 21 is Pythia new set of MPI models', 
            'MSTP(82)=5     ! MPI model', 
            'PARP(82)=2.0   ! MPI pt cutoff for multiparton interactions', 
            'PARP(89)=1800. ! MPI sqrts for which PARP82 is set', 
            'PARP(83)=1.7   ! MPI matter distrbn parameter', 
            'PARP(90)=0.26  ! MPI rescaling power', 
            'MSTP(95)=6     ! CR (color reconnection parameters)', 
            'PARP(77)=0.33  ! CR', 
            'PARP(78)=0.9   ! CR', 
            'MSTP(88)=0     ! BR', 
            'PARP(79)=2.0   ! BR', 
            'PARP(80)=0.05  ! BR', 
            'MSTP(91)=1     ! BR', 
            'PARP(91)=2.0   ! BR kt distribution', 
            'PARP(93)=10.0  ! BR'),
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
process.VtxSmeared = cms.EDFilter("GaussEvtVtxGenerator",
    VtxSmearedCommon,                                                  
    MeanX = cms.double(0.1936),                              
    MeanY = cms.double(0.168),                              
    MeanZ = cms.double(-0.29),                              
    SigmaY = cms.double(0.04),                                 
    SigmaX = cms.double(0.04),                                 
    SigmaZ = cms.double(4.51152),                              
    TimeOffset = cms.double(0.0)                               
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
    
    # ECAL SRP settings for 2009 beam commissioning

    process.simEcalDigis.ecalDccZs1stSample = cms.int32(3)
    process.simEcalDigis.dccNormalizedWeights = cms.vdouble(-1.1865, 0.0195, 0.2900, 0.3477, 0.3008, 0.2266)
    process.simEcalDigis.srpBarrelLowInterestChannelZS = cms.double(2.25*.035)
    process.simEcalDigis.srpEndcapLowInterestChannelZS = cms.double(3.75*0.06)

    return(process)



# End of customisation function definition

process = customise(process)

