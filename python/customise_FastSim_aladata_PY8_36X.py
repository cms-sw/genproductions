
def customise(process):

    process.famosSimHits.VertexGenerator.SigmaZ = 2.87
    process.famosPileUp.VertexGenerator.SigmaZ = 2.87

    process.genParticles.abortOnUnknownPDGCode = False


    # DQMStream output definition
    process.outputDQMStream = cms.OutputModule("PoolOutputModule",
                                               outputCommands = cms.untracked.vstring('drop *',
                                                                                      'keep *_MEtoEDMConverter_*_*'),
                                               fileName = cms.untracked.string('DQMStream.root'),
                                               dataset = cms.untracked.PSet(
        filterName = cms.untracked.string(''),
        dataTier = cms.untracked.string('DQM')
        )
    )
    process.outputDQMStreamOutPath = cms.EndPath(process.outputDQMStream)
    process.schedule.append( process.outputDQMStreamOutPath )



    return (process)
