import FWCore.ParameterSet.Config as cms


def customise(process):
    process.load('SimGeneral.MixingModule.mix_E7TeV_FlatDist10_2011EarlyData_50ns_PoissonOOT')
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

    process.mix.minBunch = cms.int32(-4)
    process.mix.maxBunch = cms.int32(4)
    process.mix.bunchspace = cms.int32(25)

    process.mix.input.nbPileupEvents.probFunctionVariable = cms.vint32(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10,11,12,13,14,15,16,17,18,19,20,21,22,23,24)
    process.mix.input.nbPileupEvents.probValue = cms.vdouble(
     0,
     0,
     0,
     0,
     0.007162953,
     0.121476865,
     0.232867441,
     0.276522846,
     0.250508725,
     0.101122061,
      0.010339108,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0
    )
    
    return(process)

