import FWCore.ParameterSet.Config as cms

from SimGeneral.MixingModule.mixObjects_cfi import *

def customise(process):

    process.CFWriter = cms.EDProducer("CFWriter",
                                  maxBunch = cms.int32(3),
                                  minBunch = cms.int32(-5),
                                  mixObjects = cms.PSet(
                                         mixCH = cms.PSet(
                                         ),
                                         mixTracks = cms.PSet(
                                         ),
                                         mixVertices = cms.PSet(
                                         ),
                                         mixSH = cms.PSet(
                                         ),
                                         mixHepMC = cms.PSet(
                                              mixHepMCProducts
                                         )
                                   )
    )

    process.pdigi = cms.Sequence(cms.SequencePlaceholder("randomEngineStateProducer")*cms.SequencePlaceholder("mix")*process.doAllDigi*process.trackingParticles*process.addPileupInfo*process.CFWriter)

    process.RECOSIMoutput.outputCommands.append("keep *_CFWriter_*_*")


    REDIGIInputEventSkimming= cms.PSet(
        inputCommands=cms.untracked.vstring('drop *')
        )

    GeneratorInterfaceRAWNoGenParticles = process.GeneratorInterfaceRAW.outputCommands
    for item in GeneratorInterfaceRAWNoGenParticles:
      if 'genParticles' in item:
        GeneratorInterfaceRAWNoGenParticles.remove(item) 

    REDIGIInputEventSkimming.inputCommands.extend(process.SimG4CoreRAW.outputCommands) 
    REDIGIInputEventSkimming.inputCommands.extend(GeneratorInterfaceRAWNoGenParticles) 
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
    process.fixGenInfo = cms.Path(process.GeneInfo * process.genJetMET)
    process.schedule.append(process.fixGenInfo)
    
    return(process)



