import FWCore.ParameterSet.Config as cms

def customise(process):

	process.genParticles.abortOnUnknownPDGCode = False
	process.source.numHardEvents = process.maxEvents.input

	return(process)
