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

    process.mix.minBunch = cms.int32(0)
    process.mix.maxBunch = cms.int32(0)
    process.mix.bunchspace = cms.int32(50)

    process.mix.input.nbPileupEvents.probFunctionVariable = cms.vint32(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39)
    process.mix.input.nbPileupEvents.probValue = cms.vdouble(
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
        0.001857035,
        0.111945225,
        0.100008296,
        0.138104554,
        0.117618632,
        0.114872325,
        0.103039616,
        0.115386462,
        0.10694109,
        0.047057358,
        0.043169408,
        0,
        0,
        0
      )
    
    return(process)

