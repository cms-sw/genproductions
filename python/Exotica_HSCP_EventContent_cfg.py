import FWCore.ParameterSet.Config as cms

def customise(process):
	SpecifiedEvenetContent = cms.PSet(
		outputCommands = cms.untracked.vstring(
		"keep *_g4SimHits_StoppedParticles*_*",)
		)
	process.output.outputCommands.extend(SpecifiedEvenetContent.outputCommands)
	return process
