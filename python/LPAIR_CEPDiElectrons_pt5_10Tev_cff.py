import FWCore.ParameterSet.Config as cms

source = cms.Source("MCDBSource",
        articleID = cms.uint32(310),
        supportedProtocols = cms.untracked.vstring('rfio')
)

generator = cms.EDProducer("LHEProducer",
        hadronisation = cms.PSet(
                generator = cms.string('None')
        )
)

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 0.0 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/LPAIR_CEPDiElectrons_pt5_10TeV_cff.py,v $'),
    annotation = cms.untracked.string('LPAIR GammaGamma to e+e-, pt(e) > 5 GeV at sqrt(s) = 10TeV')
)

ProductionFilterSequence = cms.Sequence(generator)

