import FWCore.ParameterSet.Config as cms

def customise(process):
        process.RandomNumberGeneratorService = cms.Service("RandomNumberGeneratorService",
                    saveFileName = cms.untracked.string(''),
theSource = cms.PSet(
    initialSeed = cms.untracked.uint32(123456789),
                    engineName = cms.untracked.string('TRandom3')
    ),

            # This is to initialize the random engines used for  Famos
            VtxSmeared = cms.PSet(
            initialSeed = cms.untracked.uint32(123456789),
                    engineName = cms.untracked.string('TRandom3')
                ),

           famosPileUp = cms.PSet(
            initialSeed = cms.untracked.uint32(918273),
                    engineName = cms.untracked.string('TRandom3')
                ),
famosSimHits = cms.PSet(
            initialSeed = cms.untracked.uint32(13579),
                    engineName = cms.untracked.string('TRandom3')
                ),

            siTrackerGaussianSmearingRecHits = cms.PSet(
            initialSeed = cms.untracked.uint32(24680),
                    engineName = cms.untracked.string('TRandom3')
                ),

            caloRecHits = cms.PSet(
            initialSeed = cms.untracked.uint32(654321),
                    engineName = cms.untracked.string('TRandom3')
                ),

            paramMuons = cms.PSet(
            initialSeed = cms.untracked.uint32(54525),
                    engineName = cms.untracked.string('TRandom3')
                ),

            l1ParamMuons = cms.PSet(
            initialSeed = cms.untracked.uint32(6453209),
                    engineName = cms.untracked.string('TRandom3')
                ),

            MuonSimHits = cms.PSet(
            initialSeed = cms.untracked.uint32(987346),
                    engineName = cms.untracked.string('TRandom3')
                ),

            simMuonRPCDigis = cms.PSet(
            initialSeed = cms.untracked.uint32(524964),
                    engineName = cms.untracked.string('TRandom3')
                ),

            simMuonCSCDigis = cms.PSet(
            initialSeed = cms.untracked.uint32(525432),
                    engineName = cms.untracked.string('TRandom3')
                ),

            simMuonDTDigis = cms.PSet(
            initialSeed = cms.untracked.uint32(67673876),
                    engineName = cms.untracked.string('TRandom3')
                ),
        evtgenproducer = cms.PSet(
                initialSeed = cms.untracked.uint32(67676)
    ))

        process.randomEngineStateProducer = cms.EDProducer("RandomEngineStateProducer")
        process.output.outputCommands.append("keep *_evtgenproducer_*_*")
        process.genParticles.src = 'evtgenproducer'
        process.genParticles.abortOnUnknownPDGCode = False
        process.famosSimHits.SourceLabel = 'evtgenproducer'

	return process
