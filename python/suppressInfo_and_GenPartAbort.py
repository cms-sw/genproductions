import FWCore.ParameterSet.Config as cms

def customise(process):
	process.MessageLogger.cerr.threshold = 'WARNING'
	process.genParticles.abortOnUnknownPDGCode = False
	
	return(process)
