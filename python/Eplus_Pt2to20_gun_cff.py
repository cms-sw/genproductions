import FWCore.ParameterSet.Config as cms

generator = cms.EDProducer('FlatRandomPtGunProducer',
	PGunParameters = cms.PSet(
		PartID = cms.vint32(-11),  ## eplus
		MinPhi = cms.double(-3.14159265359), ## in radians
		MaxPhi = cms.double(3.14159265359),
		MinEta = cms.double(-3),
		MaxEta = cms.double(3),
		MinPt = cms.double(2),
		MaxPt = cms.double(20),
	),
	Verbosity = cms.untracked.int32(0),
	psethack = cms.string('single eplus 2<Pt<20 -3<Eta<3'),
	AddAntiParticle = cms.bool(False),  ## only eplus
)

configurationMetadata = cms.untracked.PSet(
	version = cms.untracked.string('\$Revision: 1.1 $'),
        name = cms.untracked.string('\$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/Eplus_Pt2to20_gun_cff.py,v $'),
	annotation = cms.untracked.string('Fall 2010: Flat Random Pt Gun: eplus, 2<Pt<20, -3<Eta<3')
)
