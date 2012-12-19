mport FWCore.ParameterSet.Config as cms

generator = cms.EDFilter("HijingGeneratorFilter",
                                                              rotateEventPlane = cms.bool(True),
                                                              frame = cms.string('CMS     '),
                                                              targ = cms.string('P       '),
                                                              izp = cms.int32(82),
                                                              bMin = cms.double(0),
                                                              izt = cms.int32(1),
                                                              proj = cms.string('A       '),
                                                              comEnergy = cms.double(5023.0),
                                                              iat = cms.int32(1),
                                                              bMax = cms.double(15),
                                                              iap = cms.int32(208)
                                                          )

configurationMetadata = cms.untracked.PSet(
        version = cms.untracked.string('$Revision: 1.1 $'),
            annotation = cms.untracked.string('HIJING generator'),
            name = cms.untracked.string('$Source: /afs/cern.ch/project/cvs/reps/CMSSW/CMSSW/Configuration/Generator/python/Hijing_PPb_MinimumBias_cfi.py,v $')
        )
)
