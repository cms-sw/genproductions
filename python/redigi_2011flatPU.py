import FWCore.ParameterSet.Config as cms

from SimGeneral.MixingModule.mix_E7TeV_ProbDist_2010Data_BX156_cfi import *

def customise(process):
    REDIGIInputEventSkimming= cms.PSet(
        inputCommands=cms.untracked.vstring('drop *')
        )

    REDIGIInputEventSkimming.inputCommands.extend(process.SimG4CoreRAW.outputCommands) 
    REDIGIInputEventSkimming.inputCommands.extend(process.GeneratorInterfaceRAW.outputCommands) 
    REDIGIInputEventSkimming.inputCommands.extend(process.IOMCRAW.outputCommands) 

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

    # REDO the GenJets etc. in case labels have been changed
    process.load('Configuration/StandardSequences/Generator_cff')
    process.fixGenInfo = cms.Path(process.genJetMET)
    process.schedule.append(process.fixGenInfo)

    process.mix.minBunch = cms.int32(-3)
    process.mix.maxBunch = cms.int32(2)
    process.mix.bunchspace = cms.int32(50)
    process.mix.input.nbPileupEvents.probFunctionVariable = cms.vint32(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24)
    process.mix.input.nbPileupEvents.probValue = cms.vdouble(0.04593,0.04593,0.04593,0.04593,0.04593,0.04593,0.04593,0.04593,0.04593,0.04593,0.04593,0.04593,0.04593,0.04593,0.04593,0.04593,0.04593,0.04593,0.04593,0.04593,0.04593,0.01965,0.00953,0.00440,0.00196)
    
    return(process)

