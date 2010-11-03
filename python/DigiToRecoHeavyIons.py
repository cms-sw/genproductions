import FWCore.ParameterSet.Config as cms

def customise(process):
    REDIGIInputEventSkimming= cms.PSet(
        inputCommands=cms.untracked.vstring('drop *')
        )

    REDIGIInputEventSkimming.inputCommands.extend(process.SimG4CoreRAW.outputCommands)
    REDIGIInputEventSkimming.inputCommands.extend(process.GeneratorInterfaceRAW.outputCommands)
    REDIGIInputEventSkimming.inputCommands.extend(process.IOMCRAW.outputCommands)
    REDIGIInputEventSkimming.inputCommands.extend(process.HiMixRAW.outputCommands)
    REDIGIInputEventSkimming.inputCommands.extend(process.RecoGenJetsFEVT.outputCommands)

    process.source.inputCommands = REDIGIInputEventSkimming.inputCommands
    process.source.dropDescendantsOfDroppedBranches=cms.untracked.bool(False)

    process.RandomNumberGeneratorService.restoreStateLabel = cms.untracked.string('randomEngineStateProducer')

    # Remove the old RNGState product on output
    RNGStateCleaning= cms.PSet(
        outputCommands=cms.untracked.vstring('drop RandomEngineStates_*_*_*',
                                             'keep RandomEngineStates_*_*_'+process.name_())
        )

    for item in process.outputModules_().values():
        item.outputCommands.extend(RNGStateCleaning.outputCommands)

    return(process)
