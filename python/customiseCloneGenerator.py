def customiseCloneGenerator(process):
    process.generator = process.hiSignal.clone(embeddingMode=cms.bool(False))
    process.ProductionFilterSequence.replace(process.hiSignal,process.generator)
    return process
