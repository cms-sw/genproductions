import FWCore.ParameterSet.Config as cms

generator = cms.EDProducer('FlatRandomEGunProducer',
	PGunParameters = cms.PSet(
		PartID = cms.vint32(321),
		MinPhi = cms.double(-3.14159265359), ## in radians
		MaxPhi = cms.double(3.14159265359),
		MinEta = cms.double(-2.5),
		MaxEta = cms.double(2.5),
		MinE = cms.double(1   ),
		MaxE = cms.double(100 ),
	),
	Verbosity = cms.untracked.int32(0),
	psethack = cms.string('single K E 1   -100 '),
	AddAntiParticle = cms.bool(True),
)

configurationMetadata = cms.untracked.PSet(
	version = cms.untracked.string('\$Revision$'),
	name = cms.untracked.string('\$Source$'),
	annotation = cms.untracked.string('Summer2011 sample with GUN: Flat random DiKaon gun, E = 1 .. 100 GeV, no tune')
)
