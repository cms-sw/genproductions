import FWCore.ParameterSet.Config as cms

source = cms.Source("EmptySource")

from GeneratorInterface.AMPTInterface.amptDefaultParameters_cff import *
amptStringMelting = amptDefaultParameters.clone()
amptStringMelting.amptmode = cms.int32(4) 

generator = cms.EDFilter("AMPTGeneratorFilter",
                         amptStringMelting,
                         firstEvent = cms.untracked.uint32(1),
                         firstRun = cms.untracked.uint32(1),

                         comEnergy = cms.double(5020.0),
                         frame = cms.string('CMS'),                         
                         proj = cms.string('A'),
                         targ = cms.string('A'),
                         iap  = cms.int32(208),
                         izp  = cms.int32(82),
                         iat  = cms.int32(208),
                         izt  = cms.int32(82),
                         bMin = cms.double(0),
                         bMax = cms.double(30)
)

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1$'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/GeneratorInterface/AMPTInterface/python/amptDefault_cfi.py,v $'),
    annotation = cms.untracked.string('AMPT PbPb 5020 GeV Minimum Bias')
)
