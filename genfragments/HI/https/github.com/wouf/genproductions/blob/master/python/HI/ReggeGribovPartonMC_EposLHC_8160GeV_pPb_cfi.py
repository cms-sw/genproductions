import FWCore.ParameterSet.Config as cms

generator = cms.EDFilter("ReggeGribovPartonMCGeneratorFilter",

		bmin = cms.double(0), #impact parameter min in fm
		bmax = cms.double(10000),#impact parameter max in fm
		paramFileName = cms.untracked.string("Configuration/Generator/data/ReggeGribovPartonMC.param"), #file with more parameters specific to crmc interface
		skipNuclFrag = cms.bool(True), #in HI collisions nuclear fragments with pt=0 can be in the hep event. to skip those activate this option
		beammomentum = cms.double(2563),
		targetmomentum = cms.double(-6500),
		beamid = cms.int32(208),
		targetid = cms.int32(1),
		model = cms.int32(0),
		)


configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.2 $'),
    name = cms.untracked.string('$Source: /local/reps/CMSSW/CMSSW/Configuration/GenProduction/python/HI/ReggeGribovPartonMC_EposLHC_8TeV_pPb_cfi.py,v $'),
    annotation = cms.untracked.string('ReggeGribovMC generator')
    )


