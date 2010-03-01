def customise(process):
    process.schedule.remove(process.endjob_step)
    process.schedule.remove(process.out_step)
    process.options.wantSummary = cms.untracked.bool(True)
    process.MessageLogger.cerr.FwkReport.reportEvery = 1000
    process.genParticles.abortOnUnknownPDGCode = False
    return process

