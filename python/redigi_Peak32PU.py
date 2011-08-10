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

    process.mix.minBunch = cms.int32(-3)
    process.mix.maxBunch = cms.int32(2)
    process.mix.bunchspace = cms.int32(50)

    process.mix.input.nbPileupEvents.probFunctionVariable = cms.vint32(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49)
    process.mix.input.nbPileupEvents.probValue = cms.vdouble(
        0,
        9.31651e-08,
        1.09346e-06,
        5.64875e-06,
        2.361e-05,
        8.06074e-05,
        0.000225675,
        0.000558976,
        0.00120977,
        0.00234117,
        0.00412333,
        0.00670444,
        0.0100845,
        0.0142227,
        0.0190064,
        0.0240893,
        0.0293227,
        0.0343754,
        0.0390867,
        0.043246,
        0.0468416,
        0.0497495,
        0.0519864,
        0.0534802,
        0.0542481,
        0.0541758,
        0.0533452,
        0.0517044,
        0.0492733,
        0.0461293,
        0.042412,
        0.0383111,
        0.0339056,
        0.029475,
        0.0250526,
        0.0208822,
        0.0170406,
        0.0136212,
        0.0106832,
        0.00820745,
        0.00616701,
        0.00454278,
        0.00328775,
        0.00233149,
        0.00162209,
        0.0010969,
        0.000731508,
        0.000482477,
        0.000309637,
        0.000195411
        )
    
    return(process)

