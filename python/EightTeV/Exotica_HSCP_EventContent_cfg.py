import FWCore.ParameterSet.Config as cms

def stoppedHSCP(process):
	SpecifiedEvenetContent = cms.PSet(
		outputCommands = cms.untracked.vstring(
		"keep *_g4SimHits_StoppedParticles*_*",)
		)
	process.RECOSIMoutput.outputCommands.extend(SpecifiedEvenetContent.outputCommands)
	return process
