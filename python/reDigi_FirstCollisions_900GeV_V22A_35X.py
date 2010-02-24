# Auto generated configuration file
# using: 
# Revision: 1.164 
# Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/PyReleaseValidation/python/ConfigBuilder.py,v 
# with command line options: REDIGI -s DIGI,L1,DIGI2RAW,HLT:GRun -n 10 --processName REDIGI --conditions START3X_V21A::All --datatier GEN-SIM-RAW --eventcontent RAWSIM --no_exec --customise Configuration/StandardSequences/DigiToRecoNoPU.py
import FWCore.ParameterSet.Config as cms

process = cms.Process('REDIGI')

# import of standard configurations
process.load('Configuration.StandardSequences.Services_cff')
process.load('SimGeneral.HepPDTESSource.pythiapdt_cfi')
process.load('FWCore.MessageService.MessageLogger_cfi')
process.load('Configuration.StandardSequences.MixingNoPileUp_cff')
process.load('Configuration.StandardSequences.GeometryExtended_cff')
process.load('Configuration.StandardSequences.MagneticField_38T_cff')
process.load('Configuration.StandardSequences.Digi_cff')
process.load('Configuration.StandardSequences.SimL1Emulator_cff')
process.load('Configuration.StandardSequences.DigiToRaw_cff')
process.load('HLTrigger.Configuration.HLT_GRun_cff')
process.load('Configuration.StandardSequences.EndOfProcess_cff')
process.load('Configuration.StandardSequences.FrontierConditions_GlobalTag_cff')
process.load('Configuration.EventContent.EventContent_cff')

process.configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.2 $'),
    annotation = cms.untracked.string('REDIGI nevts:10'),
    name = cms.untracked.string('PyReleaseValidation')
)
process.maxEvents = cms.untracked.PSet(
    input = cms.untracked.int32(10)
)
process.options = cms.untracked.PSet(

)
# Input source
process.source = cms.Source("PoolSource",
    fileNames = cms.untracked.vstring('REDIGI_SIM.root')
)

# Output definition
process.output = cms.OutputModule("PoolOutputModule",
    splitLevel = cms.untracked.int32(0),
    outputCommands = process.RAWSIMEventContent.outputCommands,
    fileName = cms.untracked.string('REDIGI_DIGI_L1_DIGI2RAW_HLT.root'),
    dataset = cms.untracked.PSet(
        dataTier = cms.untracked.string('GEN-SIM-RAW'),
        filterName = cms.untracked.string('')
    )
)

# Additional output definition

# Other statements
process.GlobalTag.globaltag = 'START3X_V22A::All'

# Path and EndPath definitions
process.digitisation_step = cms.Path(process.pdigi)
process.L1simulation_step = cms.Path(process.SimL1Emulator)
process.digi2raw_step = cms.Path(process.DigiToRaw)
process.endjob_step = cms.Path(process.endOfProcess)
process.out_step = cms.EndPath(process.output)

# Schedule definition
process.schedule = cms.Schedule(process.digitisation_step,process.L1simulation_step,process.digi2raw_step)
process.schedule.extend(process.HLTSchedule)
process.schedule.extend([process.endjob_step,process.out_step])

# Automatic addition of the customisation function

def customise(process):
    REDIGIInputEventSkimming= cms.PSet(
        inputCommands=cms.untracked.vstring('drop *')
        )

    HLTCleaning= cms.PSet(
        inputCommands=cms.untracked.vstring('drop FEDRawDataCollection_*_*_*')
        )

    REDIGIInputEventSkimming.inputCommands.extend(process.RecoGenJetsFEVT.outputCommands)
    REDIGIInputEventSkimming.inputCommands.extend(process.RecoGenMETFEVT.outputCommands)
    REDIGIInputEventSkimming.inputCommands.extend(process.SimG4CoreRAW.outputCommands) 
    REDIGIInputEventSkimming.inputCommands.extend(process.GeneratorInterfaceRAW.outputCommands) 
    REDIGIInputEventSkimming.inputCommands.extend(process.IOMCRAW.outputCommands) 
    REDIGIInputEventSkimming.inputCommands.extend(process.HLTriggerRAW.outputCommands) 
    REDIGIInputEventSkimming.inputCommands.extend(HLTCleaning.inputCommands)
    
    process.source.inputCommands = REDIGIInputEventSkimming.inputCommands
    process.source.dropDescendantsOfDroppedBranches=cms.untracked.bool(False)
    
    process.RandomNumberGeneratorService.restoreStateLabel = cms.untracked.string('randomEngineStateProducer')
    
    # Output definition for RAW
    #process.outputRaw = cms.OutputModule("PoolOutputModule",
    #                                     outputCommands = process.RAWSIMEventContent.outputCommands,
    #                                     fileName = cms.untracked.string('New_RAWSIM.root'),
    #                                     dataset = cms.untracked.PSet(dataTier = cms.untracked.string(''),
    #                                                                  filterName = cms.untracked.string('')
    #                                                                  )
    #                                     )
    
    #process.out_step_raw = cms.EndPath(process.outputRaw)
    #process.schedule.append(process.out_step_raw)

    # Signal in Deconvolution Mode
    process.simSiStripDigis.APVpeakmode = cms.bool(True)
    process.simSiStripDigis.electronPerAdc = cms.double(262.0) #this is the value measured in peak... should we add 15%?

    return(process)


# End of customisation function definition

process = customise(process)
